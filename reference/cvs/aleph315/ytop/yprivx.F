      SUBROUTINE YPRIVX(NHXTOT,BCR,VBCR,NHX,IXHX,NEU,IXNU,
     &  NSHX,NSVHX,HXIN,VHXIN, NSNU,NSVNU,TNU,VTNU,
     &  PRVX,VPRVX,NTPVX,IXTVP,CHISQ,ITRPVX,IFL)
C
C----------------------------------------------------------*
C!    reconstructs the primary vertex
CKEY YTOP PRIMARY VERTEX
C!    Author :     G. Lutz   30/11/87
C!    Modified :   M. Bosman 01/12/88
C!    Modified :   S. Wasserbaech 09/01/90  Added NPRTR=2 procedure.
C!    Rewritten:   G. Lutz   10/01/91
C!    Modified :   M. Bosman 12/07/91
C!    Modified  :  G. Lutz   30/03/92
C!
C!    replaces routine yprimv. main difference: common fit of beam
C!    crossing and tracks
C!    exclusion of tracks with large distance from beam crossing
C!    exclusion of itc only tracks
C!
C!
C!    Description
C!    ===========
C!    This routine finds the primary vertex
C!    from beam crossing and set of track candidates
C!
C!    NHXTOT ..... TOTAL # OF TRACKS (HELICES)
C!    BCR(I) ..... X,Y,Z OF AVERAGE BEAM CROSSING POINT
C!    VBCR(I) .... CORRESPONDING VARIANCE
C!    NHX ........ NUMBER OF TRACKS (HELICES)
C!    IXHX(I) ..... TRACK INDICES
C!    NEU ........ NUMBER OF NEUTRAL TRACKS (HELICES)
C!    IXNU(I) ..... TRACK INDICES
C!    NSHX ....... SPACING BETWEEN CONSECUTIVE HELIX PARAMETERS
C!    NSVHX ...... SPACING BETWEEN CONSECUTIVE HELIX VARIANCES
C!    HXIN(I) .... BUFFER FOR HELIX PARAMETERS
C!    VHXIN(I) ... BUFFER FOR HELIX VARIANCES
C!    NSNU ....... SPACING BETWEEN CONSECUTIVE NEUTR.TR. PARAMETERS
C!    NSVHX ...... SPACING BETWEEN CONSECUTIVE NEUTR. TR. VARIANCES
C!    TNU(I)  .... BUFFER FOR NEUTRAL TRACK PARAMETERS
C!    VTNU(I)  ... BUFFER FOR NEUTRAL TRACK VARIANCES
C!    PRVX(I) .... X,Y,Z OF PRIMARY VERTEX
C!    VPRVX(I).... CORRESPONDING VARIANCES
C!    NTPVX ...... # OF TRACKS IN PRIM. VTX
C!    IXTVP ..... MARKER WORD FOR TRACKS IN VERTEX
C!    CHISQ ...... VERTEX CHISQ
C!    ITRPVX(I) .. INDICES OF TRACKS IN PRIM. VTX
C!    IFL ........ PROBLEM FLAG
C!                    0 -> successful completion
C!                   10 -> successful completion for two-track vertex
C!                 else -> failure
C!
C!---------------------------------------------------------*
#ifndef DOC
      SAVE
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"
#include "ydimto.h"
#include "yparto.h"
#include "yvxpto.h"
#include "ydumto.h"
C!---------------------------------------------------------*
C
      DIMENSION BCR(*),VBCR(*),IXHX(*),HXIN(*),VHXIN(*),
     &                         IXNU(*),TNU(*),VTNU(*),
     &                         PRVX(*),VPRVX(*),ITRPVX(*)
C
C
      DIMENSION IXTVP(*)
C
C  ARRAY FOR INDICES OF TRACKS MISSING PRIMARY VERTEX
      DIMENSION MKTRK(MAXTRK)
C
      DIMENSION JTREQ(MAXVXP)
C
C     INDICES OF NON CROSSING TRACKS RETURNED FROM YVPOSS
      DIMENSION KSNGL(MAXTRK)
C
C DIMENSION CHIVXT(MAXTRK),IPRTR(MAXTRK)
      DIMENSION CHIVXT(MAXTRK),IPRTR(MAXTRK)
C
C     TEMPORARY VERTEX
      DIMENSION WTX(3),VARWX(6)
C
C     DIMENSION MKTV1(MKDIMM)
      DIMENSION MKTV1(MKDIMM)
C
#if defined(YDEBUG)
      LOGICAL LDMP1,LDMP2
#endif
      LOGICAL LSET
C
C     LIMIT FOR IMPACT PARAMETER OF TRACKS :
C     IN R  2 CM
      DATA AIMPLR/2./
C     IN Z  5 CM
      DATA AIMPLZ/5./
C
      DATA NENTY/0/
C
#include "bmacro.h"
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
      IFL=0
C
C  ENTRY ONCE PER EVENT
C
#if defined(YDEBUG)
      IF(IDUMPP(3).GE.1) THEN
        LDMP1=.TRUE.
      ELSE
        LDMP1=.FALSE.
      ENDIF
C
      IF(IDUMPP(4).GE.1) THEN
        LDMP2=.TRUE.
      ELSE
        LDMP2=.FALSE.
      ENDIF
C
C
      IDUMPP(3)=IDUMPP(3)-1
      IDUMPP(4)=IDUMPP(4)-1
C
C     # OF ENTRIES COUNTER
      NENTY=NENTY+1
C
      ICPRI=ICPRI+1
      NCPRI=NCPRI+1
C
      IF(LDMP1)
     &  WRITE(LOUT,9997)
     &  NENTY,(BCR(I),I=1,3),SQRT(VBCR(1)),SQRT(VBCR(3)),SQRT(VBCR(6)),
     &  NHX,NEU,(IXHX(I),I=1,NHX),
     &  (IXNU(I),I=1,NEU)
 9997 FORMAT(1X,120(1H_)/
     &  ' ENTRY #',I5,' TO YPRIVX','  BCR:',3F8.3,' +-',3F8.4,
     &  ' NHX,NEU=',2I3/
     &  10X,' IXHX,IXNU:',20I3/( 21X,20I3))
#endif
C     Copy beam crossing into output vertex for the case that
C     no better vertex is found
      DO I=1,3
        PRVX(I)=BCR(I)
      ENDDO
      DO I=1,6
        VPRVX(I)=VBCR(I)
      ENDDO
      NTPVX=0
      CALL YMKZER(MKDIMM,NMSIZZ,IXTVP(1))
      CHISQ=0.
C
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,IFAIL)
      IF(IFAIL.NE.0) THEN
#if defined(YDEBUG)
        WRITE(LOUT,*) 'YPRIVX : NO FRFT BANK FOUND RETURN IER=1 '
#endif
        IER=1
        RETURN
      ENDIF
C
C     SELECT TRACKS COMPATIBLE WITH BEAM CROSSING POINT
      NPRTR=0
C
      DO 1100 I=1,NHX
        JHX=IXHX(I)
        IF(JHX.LE.0) GO TO 1100
C     REJECT TRACKS WITH LARGE Z-DISTANCE FROM BEAM CROSSING
        Z0=RTABL(KFRFT,JHX,JFRFZ0)
        IF(RTABL(KFRFT,JHX,JFRFEM+14).LT.0.) THEN
          DZ0=0.
          WRITE(LOUT,8876) JHX
 8876     FORMAT(' NEGATIVE ERROR2 FOR Z0 IN TRACK JHX=',I5)
        ELSE
          DZ0=SQRT(RTABL(KFRFT,JHX,JFRFEM+14))
        ENDIF
        TANT=RTABL(KFRFT,JHX,JFRFTL)
        COST=1./SQRT(1.+TANT**2)
        DZMAX=AIMPLR/COST
        DZMAX=SQRT(AIMPLZ**2+DZMAX**2)

#if defined(YDEBUG)
        IF(LDMP2)
     &    WRITE(LOUT,37) JHX,Z0,DZ0,AIMPLZ
   37   FORMAT(' YPRIVX  JHX=',I5,' Z0,DZ0',2F10.3,
     &    9X,'AIMPLZ', 2F10.3)
C
#endif
        IF(ABS(Z0).GT.(DZMAX+3.*DZ0)) GO TO 1100
C     REJECT TRACKS WITH LARGE R-DISTANCE FROM BEAM CROSSING
        D0=RTABL(KFRFT,JHX,JFRFD0)
        IF(RTABL(KFRFT,JHX,JFRFEM+9).LT.0.) THEN
          DD0=0.
          WRITE(LOUT,8877) JHX
 8877     FORMAT(' NEGATIVE ERROR2 FOR D0 IN TRACK JHX=',I5)
        ELSE
          DD0=SQRT(RTABL(KFRFT,JHX,JFRFEM+9))
        ENDIF
        DRMAX=AIMPLR
#if defined(YDEBUG)
        IF(LDMP2)
     &    WRITE(LOUT,38) JHX,Z0,DZ0,DZMAX,D0,DD0,DRMAX
C
#endif
        IF(ABS(D0).GT.(DRMAX+3.*DD0)) GO TO 1100
#if defined(YDEBUG)
        IF(LDMP2)
     &    WRITE(LOUT,38) JHX,Z0,DZ0,DZMAX,D0,DD0,DRMAX
   38   FORMAT(' YPRIVX  JHX=',I5,' Z0,DZ0,DZMAX',3F10.3,
     &    9X,'D0,DD0,DRMAX', 3F10.3)
#endif
C
        NPRTR=NPRTR+1
        IPRTR(NPRTR)=JHX
C
 1100 CONTINUE
C
#if defined(YDEBUG)
      IF(LDMP1) THEN
        WRITE(LOUT,8891) NPRTR,(IPRTR(J),J=1,NPRTR)
 8891   FORMAT(' YPRIVX: NPRTR=',I5,' IPRTR:',20I3)
        WRITE(LOUT,8892) NEU  ,(IXNU(J),J=1,NEU  )
 8892   FORMAT('     AND',I5,' NEUTRAL TRKS:',20I3/(26X,20I3))
      ENDIF
#endif
C
C+-+- PROTECT AGAINST THE CASE WHERE NO TRACKS COMPATIBLE
C+-+- WITH THE BEAM PROFILE ARE FOUND
      IF((NPRTR+NEU).EQ.0) THEN
        NVPOSS=0
        IFL=2
        RETURN
      ENDIF
C
C+-+-
      NGT =NHX
C
  100 CONTINUE
C+-+-
C     Special treatment for events with NPRTR=2:
C     Find primary vertex using beam crossing position
C     and its size in the xy-plane.
C     The size is set to one meter in z, so this
C     constraint is effectively removed.
C     The number of degrees of freedom in this fit
C     with two tracks is therefore 2 + 2*2 - 3 = 3.
      IF (NPRTR .EQ. 2 .AND. NEU .EQ. 0) THEN
        I1 = IPRTR(1)
        I2 = IPRTR(2)
        DO 110 I=1,3
          WTX(I) = BCR(I)
  110   CONTINUE
        DO 120 I=1,5
          VARWX(I) = VBCR(I)
  120   CONTINUE
        VARWX(6) = 100.**2
C
        CALL YFTVTR(1,2,0,.FALSE.,WTX,VARWX,
     &              IPRTR,NSHX,NSVHX,HXIN,VHXIN,
     &              IXNU,NSNU,NSVNU,TNU,VTNU,
     &              PRVX,VPRVX,CHISQ,IFAIL)
        IF(IFAIL.GT.0) THEN
          IER=1
          RETURN
        ENDIF
C
        NTPVX = 2
C       CHISQ = CHI1 + CHI2
C     CHECK FOR AN ACCEPTABLE CHISQ : maximum value 50.
        IF(CHISQ.GT.50.) THEN
          NVPOSS = 0
          IFL = 1
          RETURN
        ENDIF
        ITRPVX(1) = I1
        ITRPVX(2) = I2
        IFL = 10
        CALL YMKZER(MKDIMM,NMSIZZ,IXTVP(1))
        CALL YMKSET(MKDIMM,NMSIZZ,IXTVP(1),I1)
        CALL YMKSET(MKDIMM,NMSIZZ,IXTVP(1),I2)
        RETURN
      ENDIF
C
C     parameters for vertex search
      MXMUL=NPRTR
      JTREQ(1)=0
      NEXCL=0
C
      IF(LVBCR0) THEN
        NVX=1
        MNMUL=1
      ELSE
        NVX=0
        MNMUL=2
      ENDIF
C
      CALL YVPOSS(3,NVX,NPRTR,NEU,BCR,VBCR,
     &  IPRTR,NSHX,NSVHX,
     &  HXIN,VHXIN,
     &  IXNU,NSNU,NSVNU,TNU,VTNU,
     &  JTREQ,0,MNMUL,MXMUL,NEXCL
     &  ,NSNGL,KSNGL)
C
#if defined(YDEBUG)
      IF(LDMP2)
     &  WRITE(LOUT,9892) NVPOSS,
     &  (NTRV00(I),ITRV00(1,I),CHIVMS(I),I=1,NVPOSS)
 9892 FORMAT(' YPRIVX:',I5,' VERTEX CANDIDATES'/
     &  ('   NTRV=',I5,'  ITRV=',Z12,'  CHIVMS=',E10.3))
#endif
C
      IF(NVPOSS.LE.0) THEN
        IFL=1
        RETURN
      ENDIF
C
C     SELECT BEST VERTEX CANDIDATE
      CHMIN=1.E+30
      IVBST=0
      DO 1200 IV=1,NVPOSS
        IF(CHIVMS(IV).LT.CHMIN) THEN
          IVBST=IV
          CHMIN=CHIVMS(IV)
        ENDIF
 1200 CONTINUE
C
C
C     STORE PRIMARY VERTEX
      DO 1300 I=1,3
        PRVX(I)=VTXMS0(I,IVBST)
 1300 CONTINUE
      DO 1400 I=1,6
        VPRVX(I)=VARVMS(I,IVBST)
 1400 CONTINUE
      CALL YMKCOP(MKDIMM,ITRV00(1,IVBST),IXTVP(1))
      CHISQ=CHIVMS(IVBST)
      CALL YMKEXP(MKDIMM,NMSIZZ,ITRV00(1,IVBST),MAXTRK,NTPVX,ITRPVX)
C
C     REQUIRE MORE THAN 2 TRACKS IF NO BEAM CONSTRAINT IS REQUIRED
      IF((NTPVX+NVX).LE.2) THEN
        IFL=1
        RETURN
      ENDIF
C
#if defined(YDEBUG)
      IF(LDMP1)
     &  WRITE(LOUT,9891) (PRVX(I),I=1,3),(VPRVX(I),I=1,6),CHISQ,
     &  NTPVX,(ITRPVX(I),I=1,NTPVX)
 9891 FORMAT(' YPRIVX: ',3F8.4,3X,6E10.3,' CHISQ',E10.3/
     &  '   NTPVX=',I5,' ITRPVX:',30I3)
#endif
C
C
      RETURN
      END
#endif
