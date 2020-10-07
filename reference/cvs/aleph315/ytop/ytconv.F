      SUBROUTINE YTCONV(IER)
C
C----------------------------------------------------------*
C!    Reconstruct Gamma Conversions
CKEY YTOP
C!    Author :     G. Lutz   30/11/87
C!    Modified :   M. Bosman 01/12/88
C!    Rewritten:   G. Lutz    9/02/91
C!    Modified :   G. Lutz   30/03/92
C!    MODIFIED :   G. LUTZ    3/12/92
C!    Modified  :  W.Manner  10/02/97
C!    Modified  :  W.Manner   1/11/97 Declare WTX,VARWX arrays
C!
C!
C!    Description
C!    ===========
C!    This routine looks for gamma conversions
C!    within charged tracks identified as being electrons
C!
C!---------------------------------------------------------*
#ifndef DOC
      SAVE
#include "bcs.h"
#include "frftjj.h"
#include "fridjj.h"
#include "frtljj.h"
#include "pyerjj.h"
#include "pyfrjj.h"
#include "ynftjj.h"
#include "ynmajj.h"
#include "ynpejj.h"
#include "yntrjj.h"
#include "ybcrto.h"
#include "ydimto.h"
#include "ypafma.h"
#include "yparto.h"
#include "ydumto.h"
#include "ysumto.h"
#include "yvtxto.h"
#include "ytrkto.h"
C
C
C     GAMMA CONV. SEARCH
      REAL XYZCO(3,4),RADC1(4),RADC2(4),FIICO(4),ZETCO(4)
      DIMENSION AMPC(2),IXHX(2),VTX2(3),VARV2(6)
      DIMENSION IPTR(MAXTRK)
      DIMENSION AMHX(2),VXOUT(3),VVXOU(6),HXOU(5,2),VHXOU(15,2),
     &  PSUM(3),VPSUM(6),VPSVX(3,3),VMVX(3),VMPS(3)
      DIMENSION VXOU2(3),VVXO2(6)
      DIMENSION IXNU(2),TNUO(5,2),VTNUO(15,2)
      DIMENSION TRACK(5),VTRACK(15)
      DIMENSION VHXIN(15,MAXTRK,2)
      REAL XYZVD(3,4),RADVD(4),FVDCO(4),ZETVD(4)
      REAL XYZIT(3,20),RADIT(20),FITCO(20),ZETIT(20)
      REAL WTX(3),VARWX(6)
      DIMENSION NVDF(2),NVDB(2),NITF(2),NITB(2)
C
      LOGICAL LVAPC
      LOGICAL LMRK
      LOGICAL LGARB
      LOGICAL LCONMC
C
      LOGICAL LDMP1,LDMP2
      LOGICAL LFIRST
C
      DIMENSION ITADD(MAXTRK)
C     MAX. # OF ADDITIONAL TRACKS PASSING THROUGH CONV. VERTEX
      DATA LFIRST/.TRUE./
      DATA LDMP1/.FALSE./,LDMP2/.FALSE./
C
C     dimension of buffer for track errors
      DATA LVHXIN / 15/
C
C     maximum chisq for vertex candidates
C     maximum chisq for pointing chisq of gamma to BCRO
C     radius of vacuum
C     cut on the mass of the reconstructed particle
C     Maximum Z-distance of tracks from beam crossing
C
C     LOGICAL FLAG FOR MASS CONSTRAINT
      DATA LCONMC/.FALSE./
#if defined(YDEBUG)
C        start number for histos
      DATA IHST/20000/
#endif
C
C     MASS CHISQ LIMIT FROM OPENING ANGLE AND MASS RES.
C     DATA CHML/9./
C
C     calculate approx. vtx in YFTHVX
      DATA LVAPC/.TRUE./
C
#include "bmacro.h"
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
      IF(LFIRST) THEN
C     GENERATE MARKERS
        CALL YMKZER(1,NMSIZZ,MKEP)
        CALL YMKSET(1,NMSIZZ,MKEP,JPAFEP)
        CALL YMKSET(1,NMSIZZ,MKEP,JPAFEM)
        CALL YMKZER(1,NMSIZZ,MKPH)
        CALL YMKSET(1,NMSIZZ,MKPH,JPAFPH)
#if defined(YDEBUG)
        WRITE(LOUT,7671) MKEP,MKPH
 7671   FORMAT( ' YTCONV: MKEP=',Z10,' MKPH=',Z10 )
#endif
        LFIRST=.FALSE.
      ENDIF
C
C
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,IFAIL)
      IF(IFAIL.NE.0) GOTO 999
C
      KFRID=IW(NAMIND('FRID'))
C
      ICCON=ICCON+1
C
#if defined(YDEBUG)
      IF(IDUMPP(23).GE.1) THEN
        LDMP1=.TRUE.
      ELSE
        LDMP1=.FALSE.
      ENDIF
C
      IF(IDUMPP(24).GE.1) THEN
        LDMP2=.TRUE.
      ELSE
        LDMP2=.FALSE.
      ENDIF
C
      IDUMPP(23)=IDUMPP(23)-1
      IDUMPP(24)=IDUMPP(24)-1
C
C
      IF(LDMP1)
     &  WRITE(LOUT,9997) ICCON
 9997 FORMAT(1X,120(1H_)/
     &  ' ENTRY #',I5,' TO YTCONV')
#endif
C
C
      IER = 0
C
C-- Add multiple scattering component to ITC-TPC track errors
C-- in case we deal with FRFT bank 0
C-- and copy them to an array
C
C multiple scattering in THE FOLLOWING SECTION is slightly incorrect
C when e.g. vertex is in ITC but no ITC hits are found (FRFT0 gets
C errors at first measured point on track)
      IF(KFRFT0.EQ.0) THEN
        DO I=1,LROWS(KFRFT)
          IF(I.LE.MAXTRK) THEN
            IFRFT=KROW(KFRFT,I)
            DO J=1,15
              VHXIN(J,I,1)=RW(IFRFT+JFRFEM+J-1)
            ENDDO
          ENDIF
        ENDDO
      ELSE
        DO 40 I=1,LROWS(KFRFT)
          IF(I.GT.MAXTRK) GOTO 40
          IFRFT=KROW(KFRFT,I)
          DO 41 J=1,15
   41     VHXIN(J,I,1)=RW(IFRFT+JFRFEM+J-1)
          IF(KFRFT.EQ.KFRFT0) THEN
            NITC=ITABL(KFRTL,I,JFRTNI)
            CALL UMSERR(1,NITC,0,RW(KFRFT+LMHLEN+JFRFIR),
     &                VHXIN(1,I,1),VHXIN(1,I,1),IFAIL)
            IF(IFAIL.NE.0) GOTO 998
          ENDIF
   40   CONTINUE
      ENDIF
C
C-- save ITC-TPC-VDET track errors
C-- check that VDET track bank exists
      IF(KFRFT.EQ.KFRFT0) GOTO 44
      DO 42 I=1,LROWS(KFRFT)
        IFRFT=KROW(KFRFT,I)
        IF(I.GT.MAXTRK) GOTO 42
        DO 43 J=1,15
   43   VHXIN(J,I,2)=RW(IFRFT+JFRFEM+J-1)
   42 CONTINUE
   44 CONTINUE
C
C find gamma conversions
C     select tracks compatible with e+- assignement
      II=0
      DO 50 K=1,NGTRTO
        I=IPTRTO(K)
        IF(I.EQ.0) GOTO 50
C     reject itc only tracks
        KODE=1
        IF(ITABL(KFRTL,I,JFRTNT)+ITABL(KFRTL,I,JFRTNV).EQ.0) GO TO 49
C     reject tracks with large z-distance from beam crossing
        Z0=RTABL(KFRFT,I,JFRFZ0)
        DZ0=SQRT(RTABL(KFRFT,I,JFRFEM+14))
        KODE=3
        IF(ABS(Z0).GT.(DZMXCO+3.*DZ0+3*SQRT(VBCROS(6)))) GO TO 50
C     reject non electrons
        KODE=4
        CALL YMKAND(1,NMSIZZ,MKEP,KPIDF0(I),IDUM,LMRK)
        IF(.NOT.LMRK) THEN
          II=II+1
          IPTR(II)=IPTRTO(K)
        ELSE
          GO TO 49
        ENDIF
        GO TO 50
   49   CONTINUE
#if defined(YDEBUG)
        IF(LDMP2) WRITE(LOUT,*) ' YTCONV: TRK ',I,
     +' REJECTED CODE = (1=ITC 2=GAMMA 3=Z DIST 4=ID)',KODE
#endif
   50 CONTINUE
C
#if defined(YDEBUG)
      IF(LDMP1) THEN
        WRITE(LOUT,51) II,(IPTR(K),K=1,II)
   51   FORMAT(' YTCONV: gamma conv. search with',I3,' tracks:',10I3/
     &    (34X,10I3))
      ENDIF
#endif
C     LOOP OVER PAIRS WITH OPPOSITE CHARGE
      IF(II.LT.2) GO TO 61
      II1=II-1
      DO 60 I=1,II1
        I1=I+1
        IIP=IPTR(I)
        DO 60 J=I1,II
          IP=IIP
          JJP=IPTR(J)
          JP=JJP
C       require opposite charge
          IF(PRECTO(IP)*PRECTO(JP).GT.0.) GO TO 60
          IF(PRECTO(IP).LT.0.) THEN
C     TAKE POSITIVE PARTICLE FIRST
            IP=JJP
            JP=IIP
          ENDIF
#if defined(YDEBUG)
          IF(LDMP2) WRITE(LOUT,67) IIP,JJP,IP,JP
   67     FORMAT(' YTCONV: try trks',2I5, ' pos.,neg.',2I5)
#endif
          IXHX(1)=IP
          IXHX(2)=JP
C
C     electron probability
          ELPR1=RTABL(KFRID,IP,JFRIPE)
          ELPR2=RTABL(KFRID,JP,JFRIPE)
          IF(AMIN1(ELPR1,ELPR2).LT.EPLOCO.OR.
     &      AMAX1(ELPR1,ELPR2).LT.EPHICO) THEN
#if defined(YDEBUG)
            IF(LDMP2)
     +        WRITE(LOUT,68) ELPR1,ELPR2,EPLOCO,EPHICO
   68       FORMAT(' YTCONV: rejection electron prob.',
     &        2F7.3,' EPLOCO,EPHICO',2F7.3)
#endif
            GO TO 60
          ENDIF
C
C  FIT VERTEX ONLY
          KFRFTU=KFRFT
          IF(KFRFT0.NE.0) KFRFTU=KFRFT0
C CHECK IF VDET TRACKS AVAILABLE
          KMAL=2
          IF(KFRFT.EQ.KFRFT0) KMAL=1
          KOFRF=1
          DO 160 MAL=1,KMAL
C  FIRST FIT VERTEX WITH FRFT NUMBER 0 IE TRACKS WITHOUT VDET HITS
#if defined(YDEBUG)
            IF(LDMP2)
     &        WRITE(LOUT,*)
     &' YTCONV: no VDET (MAL=1) with VDET (MAL=2) fit YFTVTR MAL=',MAL
#endif
C     conversion vertex
            CALL YFTVTR(0,2,0,LVAPC,DUM,DUMY,IXHX,
     &        LCOLS(KFRFTU),LVHXIN,
     &        RW(KFRFTU+LMHLEN+JFRFIR),VHXIN(1,1,MAL),
     &        IDUM,IDUM,IDUM,DUM,DUM,VTX2,VARV2,CHVX2,IFAIL)
C
C NOW LOOK IF WE HAVE VDET HITS BEHIND THE VERTEX
C BUT ONLY IN THE CASE WE REQUEST USING FRFT nr 2 BANK
            IF(MAL.EQ.2.OR.KMAL.EQ.1) GO TO 160
            RVE=SQRT(VTX2(1)**2+VTX2(2)**2)
            CALL YVDCOF(IXHX(1),NUMC1,XYZCO,RADC1,FIICO,ZETCO,NCOM1,
     &        LDMP1)
            CALL YVDCOF(IXHX(2),NUMC2,XYZCO,RADC2,FIICO,ZETCO,NCOM2,
     &        LDMP1)
#if defined(YDEBUG)
            IF(LDMP2) WRITE(LOUT,*)' YTCONV: VERTEX RADIUS ',RVE
            IF(LDMP2.AND.NUMC1.GT.0)WRITE(LOUT,*)
     &        ' YTCONV: coord rad 1rst track ',RADC1(NUMC1)
            IF(LDMP2.AND.NUMC2.GT.0)WRITE(LOUT,*)
     &        ' YTCONV: coord rad 2nd  track ',RADC2(NUMC2)
#endif
C IF BOTH TRACKS HAVE 2 COMPLETE HITS CONTINUE IN ANY CASE
            IF(NCOM1.GE.2.AND.NCOM2.GE.2) GO TO 59
C OTHERWISE, IF THE VDET HITS ARE IN FRONT OF THE FOUND VTX IGNORE THEM
C     IF(RADC1(NUMC1).GT.RVE.AND.NUMC1.GT.0) GO TO 59
C     IF(RADC2(NUMC2).GT.RVE.AND.NUMC2.GT.0) GO TO 59
C     GO TO 161
C IF BOTH TRACKS HAVE NO HIT INFORMATION (MINI) REFIT IF RADIUS < 6 CM
            IF(NCOM1.EQ.-1.AND.NCOM2.EQ.-1) THEN
              IF(RVE.LE.6.) GO TO 59
              GO TO 61
            ENDIF
C OTHERWISE, IF THE VDET HITS ARE IN FRONT OF THE FOUND VTX IGNORE THEM
            IF(NUMC1.GT.0 .AND. RADC1(1).LT.RVE) GOTO 61
            IF(NUMC2.GT.0 .AND. RADC2(1).LT.RVE) GOTO 61
            IF(NUMC1.EQ.0 .AND. NUMC2.EQ.0) GO TO 61
   59       CONTINUE
C REFIT INCLUDING VDET HITS
            KFRFTU=KFRFT
            KOFRF=2
  160     CONTINUE
  161     CONTINUE
#if defined(YDEBUG)
          IF(LDMP2) THEN
            WRITE(LOUT,8778) (IXHX(IJK),IJK=1,2),
     &        (VTX2(IJK),IJK=1,3),(VARV2(IJK),IJK=1,6),CHVX2,IFAIL
 8778       FORMAT(' YTCONV: ret.YFTVTR IXHX=',2I2,
     &        ' VTX2=',3F9.3/,'  VARV2=',6E9.3,' CHVX2=',F8.1,
     &        ' IFAIL=',I3 )
          ENDIF
          CALL HFILL(IHST+1041,CHVX2,0.,1.)
#endif
          IF(IFAIL.NE.0.OR.CHVX2.GT.CHVXCO) GO TO 60
C
C  REQUIRE VERTEX OUTSIDE VACUUM
          RVX=SQRT(VTX2(1)**2+VTX2(2)**2)
#if defined(YDEBUG)
          CALL HFILL(IHST+1061,RVX,0.,1.)
          IF(RVX.LT.RVACCO) THEN
            IF(LDMP2) WRITE(LOUT,*)
     &        ' YTCONV: Radius of decay vertex inside vacuum :',RVX
          ELSE
            IF(LDMP2) WRITE(LOUT,*)
     &        ' YTCONV: Radius of decay vertex outside vacuum :',RVX
          ENDIF
#endif
          IF(RVX.LT.RVACCO) GOTO 60
C
          NPIDC=1
          AMPC(1)=YPMASS(JPAFEP)
          AMPC(2)=YPMASS(JPAFEM)
C
C     conversion vertex complete fit
          CALL YFMVTR(0,2,0,.FALSE.,.TRUE.,.TRUE.,.TRUE.,
     &      VTX2,VARV2,IXHX,
     &      LCOLS(KFRFT),LVHXIN,
     &      RW(KFRFT+LMHLEN+JFRFIR),VHXIN(1,1,KOFRF),
     &      IXNU,
     &      NSNU,NSVNU,TNUI,VTNUI,
     &      NPIDC,AMPC,
     &      VXOUT,VVXOU,HXOU,VHXOU,TNUO,VTNUO,
     &      PSUM,VPSUM,VPSVX,
     &      AMASS,DMASS,VMVX,VMPS,
     &      CHISQ,IFAIL)
C
#if defined(YDEBUG)
          IF(LDMP2) THEN
            WRITE(LOUT,8779) CHISQ,IFAIL,AMASS,DMASS,
     &        (VXOUT(IJK),SQRT(VVXOU(IJK*(IJK+1)/2)),IJK=1,3)
 8779       FORMAT(' YTCONV: return YFMVTR; CHISQ=',F10.3,
     &        ' IFAIL=',I3,' AMASS=',F10.3,'+-',F6.3/
     &        ' VXOUT=',3(F9.4,'+-',F7.4))
          ENDIF
#endif
C  FIRST CHECK CHISQ AGAIN
          IF(IFAIL.NE.0.OR.CHISQ.GT.CHVXCO) GO TO 60
#if defined(YDEBUG)
          CALL HFILL(IHST+1081,AMASS,0.,1.)
#endif
C
          P1=ABS(PRECTO(IXHX(1)))
          P2=ABS(PRECTO(IXHX(2)))
          POP=P1/P2+P2/P1
          AM02=YPMASS(JPAFEP)**2*(2.+POP)
          AMTH2=YPMASS(JPAFEP)**2*POP
C LIMIT DMASS TO 1 GEV
          DMASS=MIN(1.,DMASS)
C------ CALCULATED ERRORS ON THE MASS IN THE CASE OF PHOTON
C       CONVERSIONS ARE NOT REALLY CORRECT IN THE LINEAR
C       APPROXIMATION USED IN THE YTOP METHOD
C       IF((AMASS**2-AM02).GT.CHML*(AMTH2+DMASS**2)) ....
C       USE A FIX CUT ON THE MASS INSTEAD OF A CUT A CERTAIN
C       NUMBER OF SIGMAS
          IF(AMASS.GT.AMCTCO) THEN
#if defined(YDEBUG)
            IF(LDMP2) THEN
              WRITE(LOUT,53) AMASS,DMASS,AMCTCO
   53         FORMAT(' YTCONV: Mass',F8.3,'+-',F8.3,
     &          'OUTSIDE LIMIT',1F8.3)
            ENDIF
#endif
            GO TO 60
          ELSE
#if defined(YDEBUG)
            IF(LDMP2) THEN
              WRITE(LOUT,54) AMASS,DMASS,AMCTCO,AM02,AMTH2
   54         FORMAT(' YTCONV: Mass',E10.3,'+-',E10.3,
     &          'INSIDE LIMIT',F8.3,' AM02,AMTH2',2E10.3)
            ENDIF
            CALL HFILL(IHST+1091,AMASS,0.,1.)
            CALL HFILL(IHST+1071,RVX,0.,1.)
            IF(LDMP2) THEN
              WRITE(LOUT,55) (IXHX(K),K=1,2),(VTX2(K),K=1,3),
     &           AMPC(1),AMPC(2),AMASS
   55         FORMAT(' YTCONV: IXHX=',2I3,' VTX',3F10.4,' AMASS=',
     &          3(2F8.3,1F10.3))
            ENDIF
#endif
C
            IF(LCONMC) THEN
C
C  MASS CONSTRAINED VERTEX FIT
              AMCON=AM02
              DMQCON=AMAX1(AMTH2,.0001)
              CALL YFVMC(0,2,0,.FALSE.,
     &          VTX2,VVXIN,IXHX,
     &          LCOLS(KFRFTU),LVHXIN,
     &          RW(KFRFTU+LMHLEN+JFRFIR),VHXIN(1,1,KOFRF),
     &          IXNU,
     &          NSNU,NSVNU,TNUI,VTNUI,
     &          AMPC(1),AMCON,DMQCON,
     &          VXOUT,VVXOU,HXOU,VHXOU,TNUO,VTNUO,
     &          PSUM,VPSUM,VPSVX,
     &          AMASS,DMASS,
     &          CHISQ,IFAIL)
C
#if defined(YDEBUG)
              IF(LDMP2) THEN
                WRITE(LOUT,8789) CHISQ,IFAIL,AMASS,DMASS,
     &            (VXOUT(IJK),SQRT(VVXOU(IJK*(IJK+1)/2)),IJK=1,3)
 8789           FORMAT(' YTCONV: return YFVMC ; CHISQ=',F10.3,
     &            ' IFAIL=',I3,' AMASS=',F10.3,'+-',F6.3/
     &            ' VXOUT=',3(F9.4,'+-',F7.4))
              ENDIF
#endif
C
              IF(IFAIL.NE.0.OR.CHISQ.GT.CHVXCO) GO TO 60
C
            ENDIF
C
            CALL YTPAR(0,VXOUT,VVXOU,PSUM,VPSUM,VPSVX,
     &        TRACK,VTRACK,IFAIL)
#if defined(YDEBUG)
            IF(LDMP2.AND.IFAIL.NE.0)WRITE(LOUT,*)
     &        ' YTCONV: YTPAR failed:',IFAIL
#endif
            IF(IFAIL.GT.0) GOTO 60
C
C POINTING CHISQ OF RECONSTRUCTED GAMMA W.R.T BCRO
            IXNU(1)=1
            CALL YFTVTR(1,0,1,.FALSE.,BCROSS,VBCROS,
     &        IDUM,IDUM,IDUM,
     &        DUM,DUM,
     &        IXNU,5,15,TRACK,VTRACK,VXOU2,VVXO2,CHIS2,IFAIL)
#if defined(YDEBUG)
            IF(LDMP2.AND.IFAIL.NE.0)
     &        WRITE(LOUT,*)' YTCONV: CHI**2 pointing fit failed :',IFAIL
#endif
            IF(IFAIL.GT.0)GOTO 60
#if defined(YDEBUG)
            IF(LDMP2)WRITE(LOUT,*)' YTCONV: CHI**2 pointing = ',CHIS2
            CALL HFILL(IHST+1191,CHIS2,0.,1.)
#endif
C-- CUT ON THE POINTING CHISQ (2 D.O.F.)
            IF(CHIS2.GT.CHPTCO) GOTO 60
C
C     vertex distance
            VDIST=SQRT((VXOUT(1)-VXOU2(1))**2
     &          +(VXOUT(2)-VXOU2(2))**2
     &          +(VXOUT(3)-VXOU2(3))**2)
C
C     vertex dist. chisq (refit decay tracks to handle nonlinear probl.)
            CALL YFTVTR(1,2,0,.FALSE.,BCROSS,VBCROS,IXHX,
     &        LCOLS(KFRFTU),LVHXIN,
     &        RW(KFRFTU+LMHLEN+JFRFIR),VHXIN(1,1,KOFRF),
     &        IDUM,IDUM,IDUM,DUM,DUM,WTX,VARWX,CHIVS,IFAIL)
C
C     REJECT CONV.WITH ADDITIONAL TRACKS PASSING THROUGH VERTEX
C
            NADD=0
            DO 70  KK=1,II
              KP=IPTR(KK)
              IF(IP.NE.KP.AND.JP.NE.KP) THEN
                CALL YFTVTR(1,1,0,.FALSE.,VTX2,VARV2,KP,
     &            LCOLS(KFRFTU),LVHXIN,
     &            RW(KFRFTU+LMHLEN+JFRFIR),VHXIN(1,1,KOFRF),
     &            IDUM,IDUM,IDUM,DUM,DUM,WTX,VARWX,CHISA,IFAIL)
                IF(CHISA.LT.10.     ) THEN
                  NADD=NADD+1
                  ITADD(NADD)=KP
                  IF(NADD.GT.NAMXCO) THEN
#if defined(YDEBUG)
                    IF(LDMP1) THEN
                      WRITE(LOUT,8791) (IXHX(IJK),IJK=1,2),CHISA,
     &                  (VTX2(IJK),IJK=1,3),
     &                  NADD,NAMXCO,(ITADD(IJK),IJK=1,NADD)
 8791                 FORMAT(' YTCONV: IXHX=',2I3,' CHISA=',E10.1,
     &                  ' VTX',3F8.2/
     &                  5X,' NADD=',I3,' NAMXCO=',I3,
     &                  ' ADD.TRKS IN VTX:',10I3)
                    ENDIF
#endif
                    GO TO 60
                  ENDIF
                ENDIF
              ENDIF
   70       CONTINUE
C
C      set particle origin flag
            CALL YMKSET(1,NMSIZZ,KPORF0(IP),JPAFPH)
            CALL YMKSET(1,NMSIZZ,KPORF0(JP),JPAFPH)
C
#if defined(YDEBUG)
            IF(LDMP1) THEN
              WRITE(LOUT,56) (IXHX(K),K=1,2),KPORF0(IP),KPORF0(JP),
     &          VTX2(1),SQRT(VARV2(1)),VTX2(2),SQRT(VARV2(3)),
     &          VTX2(3),SQRT(VARV2(6)),CHVX2,
     &          VXOUT(1),SQRT(VVXOU(1)),VXOUT(2),SQRT(VVXOU(3)),
     &          VXOUT(3),SQRT(VVXOU(6)),CHISQ,
     &          AMASS,DMASS,PRECTO(IXHX(1)),PRECTO(IXHX(2)),
     &          SQRT(AM02),SQRT(AMTH2)
   56         FORMAT(' YTCONV: gamma conv. found: IXHX=',2I3,' KPORF0=',
     &          2Z10/'  VTX2',1F10.4,'+-',F7.4,F10.4,'+-',F7.4,F10.4,
     &          '+-',F7.4,' CHVX2=',F7.4/
     &          '  VTX ',1F10.4,'+-',F7.4,F10.4,'+-',F7.4,F10.4,
     &          '+-',F7.4,' CHISQ=',F7.4/
     &          '  AMASS=',F10.3,'+-',F6.3,'  P1,P2',2F8.1,' AM0,AMTH',
     &          2F8.3)
            ENDIF
            CALL HFILL(IHST+1291,AMASS,0.,1.)
            CALL HFILL(IHST+1391,RVX,0.,1.)
#endif
C
C
C-- SAVE THE RECONSTRUCTED CONVERSION-VERTEX IN THE BANK PYER
            LGARB=.FALSE.
C----- output to BOS-BANK PYER
C           KPYER=IW(NAMIND('PYER'))
            KPYER=NLINK('PYER',0)
            IF(KPYER.GT.0) THEN
C  --- BANK ALREADY EXISTS
              KLAST = LROWS(KPYER)+1
            ELSE
              KLAST = 1
            ENDIF
            KYWI  = LPYERA*KLAST
C  --- WE BOOK HERE THE SPACE FOR THE BANK
            CALL AUBOS('PYER',0,LMHLEN+KYWI,KPYER,IRET)
C  --- ? NO SPACE
            IF(IRET.EQ.2) GOTO 997
            IF(IRET.EQ.1) LGARB=.TRUE.
            IW(KPYER+LMHCOL) = LPYERA
            IW(KPYER+LMHROW) = KLAST
C  ---?
            IPYER = KROW(KPYER,KLAST)
C  --- STORE INFORMATION
C  --- TYPE OF VERTEX 0..255 1=MAIN 2=V0,3=MAIN FOR 2-PRONGS
C                                   4=CONVERSION
            IW(IPYER+JPYETY)      = 4
C-- copy the vertex position
            CALL UCOPY(VXOUT(1),RW(IPYER+JPYEVX),3)
C-- copy the variances
C----- covariance matrix 1 2 4
C                          3 5
C                            6
            CALL UCOPY(VVXOU(1),RW(IPYER+JPYEVM),6)
C-- copy the chisq
C----- C2 Chisquare 0.0 ...255.
            RW(IPYER+JPYEC2) = CHISQ
C-- copy the number of degrees of freedom,
C                    2x2 for each track - 3 for vertex constraint
            IW(IPYER+JPYEDF) = 1

C-- save the track indices belonging to the secondary vertex
C-- in the bank PYFR
            KPYFR=IW(NAMIND('PYFR'))
            IF(KPYFR.GT.0) THEN
C----- bank already exists
              NRPYF = LROWS(KPYFR)+2
            ELSE
              NRPYF = 2
            ENDIF
            CALL AUBOS('PYFR',0,LMHLEN+LPYFRA*NRPYF,KPYFR,IRET)
            IF(IRET.EQ.2) GOTO 996
            IF(IRET.EQ.1) LGARB=.TRUE.
            IW(KPYFR+LMHCOL) = LPYFRA
            IW(KPYFR+LMHROW) = NRPYF
            DO 300 ITR = 1,2
              IPYFR = KROW(KPYFR,ITR+NRPYF-2)
C  --- vertex number
              IW(IPYFR+JPYFVN) = KLAST
C  --- track number
              IW(IPYFR+JPYFTN) = IXHX(ITR)
  300       CONTINUE
C
C-- save the incoming photon-track in the bank ynft
C      output to bos-bank pyer
            KYNFT=IW(NAMIND('YNFT'))
            IF(KYNFT.GT.0) THEN
C      bank already exists
              KLAST = LROWS(KYNFT)+1
            ELSE
              KLAST = 1
            ENDIF
            KYWI  = LYNFTA*KLAST
C
C  INDEX OF NEUTRAL TRACK
            INU=KLAST
C  SET PARTICLE IDENTIFICATION FLAG
            JP=INU+MAXHLX
            CALL YMKORR(1,NMSIZZ,KPIDF0(JP),JPAFPH,KPIDF0(JP))
C
C      we book here the space for the bank
            CALL AUBOS('YNFT',0,LMHLEN+KYWI,KYNFT,IRET)
C      ? no space
            IF(IRET.EQ.2) GOTO 995
            IF(IRET.EQ.1) LGARB=.TRUE.
            IW(KYNFT+LMHCOL) = LYNFTA
            IW(KYNFT+LMHROW) = KLAST
C
            IYNFT = KROW(KYNFT,KLAST)
C      store information
C-- copy the track parameters
            CALL UCOPY(TRACK(1),RW(IYNFT+JYNFMO),5)
C-- copy the variances
C      covariance matrix 1 2 4 7 11
C                          3 5 8 12
C                            6 9 13
C                             10 14
C                                15
            CALL UCOPY(VTRACK(1),RW(IYNFT+JYNFEM),15)
C-- copy the chisq
C      C2 Chisquare
            RW(IYNFT+JYNFC2) = CHISQ
C-- copy the # of deg. of freedom, 2 x ntr - 3 (+1 for mass constraint)
            IW(IYNFT+JYNFDF) = 1
            IF(LCONMC) IW(IYNFT+JYNFDF) = 2
C
C-- charge
            IW(IYNFT+JYNFCH) = 0
C
C-- number of daughter tracks
            IW(IYNFT+JYNFND) = 2
C-- number of mass assignements
            IW(IYNFT+JYNFNM) = 1
C
C-- fill daughter track bank  YNTR
            KYNTR=IW(NAMIND('YNTR'))
            IF(KYNTR.GT.0) THEN
C      bank already exists
              KLAST = LROWS(KYNTR)+2
            ELSE
              KLAST = 2
            ENDIF
            KYWI  = LYNTRA*KLAST
C      we book here the space for the bank
            CALL AUBOS('YNTR',0,LMHLEN+KYWI,KYNTR,IRET)
C      ? no space
            IF(IRET.EQ.2) GOTO 995
            IF(IRET.EQ.1) LGARB=.TRUE.
            IW(KYNTR+LMHCOL) = LYNTRA
            IW(KYNTR+LMHROW) = KLAST
C
            IYNTR=KROW(KYNTR,KLAST-1)
C      store information
            DO 330 ITR=1,2
C-- save the mother track number
              IW(IYNTR+LYNTRA*(ITR-1)+JYNTMT) = LROWS(KYNFT)
C-- save the daughter track number
              IW(IYNTR+LYNTRA*(ITR-1)+JYNTDT) = IXHX(ITR)
C-- save the daughter track type
              IW(IYNTR+LYNTRA*(ITR-1)+JYNTTT) = 1
  330       CONTINUE
C-- save the mass assign to the daughter using ALEPH particle table
C-- positive particle comes in first position
            IW(IYNTR+JYNTMA)= 2
            IW(IYNTR+LYNTRA+JYNTMA) = 3
C
C-- fill  mass assignment bank  YNMA
            KYNMA=IW(NAMIND('YNMA'))
            IF(KYNMA.GT.0) THEN
C      bank already exists
              KLAST = LROWS(KYNMA)+1
            ELSE
              KLAST = 1
            ENDIF
            KYWI  = LYNMAA*KLAST
C      we book here the space for the bank
            CALL AUBOS('YNMA',0,LMHLEN+KYWI,KYNMA,IRET)
C      ? no space
            IF(IRET.EQ.2) GOTO 995
            IF(IRET.EQ.1) LGARB=.TRUE.
            IW(KYNMA+LMHCOL) = LYNMAA
            IW(KYNMA+LMHROW) = KLAST
C
            IYNMA=KROW(KYNMA,KLAST)
C      store information
C-- save the mother track number
            IW(IYNMA+JYNMMT) = LROWS(KYNFT)
C-- save the particle assignment : gamma
            IW(IYNMA+JYNMPA) = 1
C-- save the mass
            RW(IYNMA+JYNMMA) = AMASS
C-- save the error on the mass
            RW(IYNMA+JYNMEM) = DMASS
C-- save the vertex distance
            RW(IYNMA+JYNMVD) = VDIST
C-- save the chisq vertex separation
            RW(IYNMA+JYNMVS) = CHIVS
C-- save the impact parameter chisq
            RW(IYNMA+JYNMIC) = CHIS2
C-- save the numb. of add. tracks in conv. vertex
            IW(IYNMA+JYNMNA) = NADD
C-- save the electron probabilities
            RW(IYNMA+JYNMPR) = ELPR1
            RW(IYNMA+JYNMPR+1) = ELPR2
C
C-- fill 3-momentum bank  YNPE
            KYNPE=IW(NAMIND('YNPE'))
            IF(KYNPE.GT.0) THEN
C      bank already exists
              KLAST = LROWS(KYNPE)+1
            ELSE
              KLAST = 1
            ENDIF
            KYWI  = LYNPEA*KLAST
C      we book here the space for the bank
            CALL AUBOS('YNPE',0,LMHLEN+KYWI,KYNPE,IRET)
C      ? no space
            IF(IRET.EQ.2) GOTO 995
            IF(IRET.EQ.1) LGARB=.TRUE.
            IW(KYNPE+LMHCOL) = LYNPEA
            IW(KYNPE+LMHROW) = KLAST
C
            IYNPE=KROW(KYNPE,KLAST)
C      store information
            CALL UCOPY(PSUM,RW(IYNPE+JYNPPX),3)
C
C
C-- save relative pointers in between banks
C-- watch for garbage collection
            IF(LGARB) THEN
C       KPYER=IW(NAMIND('PYER'))
              KPYER=NLINK('PYER',0)
              KYNFT=IW(NAMIND('YNFT'))
              KYNTR=IW(NAMIND('YNTR'))
              KYNMA=IW(NAMIND('YNMA'))
            ENDIF
            LPYER=LROWS(KPYER)
            IPYER=KROW(KPYER,LPYER)
            LYNFT=LROWS(KYNFT)
            IYNFT=KROW(KYNFT,LYNFT)
            LYNMA=LROWS(KYNMA)
            IYNMA=KROW(KYNMA,LYNMA)
            LYNTR=LROWS(KYNTR)
            IYNTR=KROW(KYNTR,LYNTR-1)
            IW(IYNFT+JYNFPV)=LPYER
            IW(IYNFT+JYNFPM)=LYNMA
            IW(IYNFT+JYNFPT)=LYNTR-1
            DO 351 ITR=1,2
  351       IW(IYNTR+LYNTRA*(ITR-1)+JYNTMT)=LYNFT
C  --- end of bank output
#if defined(YDEBUG)
            IF(LDMP2)THEN
C      print the banks with conversion information
              WRITE(LOUT,2345)
 2345         FORMAT(' YTCONV: print banks')
              CALL PRTABL('PYER',0)
              CALL PRTABL('PYFR',0)
              CALL PRTABL('YNFT',0)
              CALL PRTABL('YNMA',0)
              CALL PRTABL('YNPE',0)
              CALL PRTABL('YNTR',0)
            ENDIF
#endif
C
          ENDIF
C
C--   fill the summary information
          NRCOTO = NRCOTO + 1
          PPS =  SQRT(PSUM(1)**2+PSUM(2)**2+PSUM(3)**2)
          AMCOTO = AMCOTO + PPS
          ACCOTO = ACCOTO + CHISQ
          ARCOTO = ARCOTO + SQRT(VXOUT(1)**2+VXOUT(2)**2)
C
   60 CONTINUE
   61 CONTINUE
C
      RETURN
C
  995 CALL ALTELL('YTCONV :  no space to create bank YNFT IER=1',0,
     &   ' RETURN ')
      GOTO 1000
  996 CALL ALTELL('YTCONV :  no space to create bank PYFR IER=1',0,
     &   ' RETURN ')
      GOTO 1000
  997 CALL ALTELL('YTCONV :  no space to create bank PYER IER=1',0,
     &   ' RETURN ')
      GOTO 1000
  998 CALL ALTELL('YTCONV: problem in UMSERR IER=1',0,' RETURN ')
      GOTO 1000
  999 CONTINUE
#if defined(YDEBUG)
      WRITE(LOUT,*) 'YTCONV: THE POINTER KFRFT IS 0 RETURN IER=1'
#endif
 1000 IER=1
      RETURN
      END
#endif
