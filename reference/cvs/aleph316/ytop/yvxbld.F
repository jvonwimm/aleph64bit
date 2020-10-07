      SUBROUTINE YVXBLD(NHX,NEU,IXHX,IXNU,
     &  HXI,NSHX,VHXI,NSVHX,
     &  TNU,NSNU,VTNU,NSVNU,
     &  VTX1,VVTX1,CHIV1,NDFV1,
     &  MKTV1,VTX2,VVTX2,CHVX2,NDFV2,MKTV2,ICONL,
     &  NADTR,IXTRI,CHVXI)
C
C----------------------------------------------------------*
C!    build up vertex by adding sequentially tracks
CKEY YTOP VERTEX
C!    Author :     G. Lutz   30/11/87
C!    Modified :   M. Bosman 01/12/88
C!REWRITTEN:   G. LUTZ   JULY 91
C!    corrected 6.2.92 G.Lutz
C!
C!    Description
C!    ===========
C!    builds up a vertex by adding sequentially
C!    the most compatible track
C!
C!    NHX ......... NUMBER OF CHARGED TRACKS TO BE INVESTIGATED
C!    NEU ......... NUMBER OF NEUTRAL TRACKS TO BE INVESTIGATED
C!    IXHX(I) ..... INDICES OF HELICES
C!    IXNU(I) ..... INDICES OF NEUTRAL TRACKS
C!    HXI(I) ...... HELIX PARAMETERS
C!    NSHX ....... SPACING BETWEEN CONSECUTIVE HELICES
C!    VHXI(I) ..... HELIX VARIANCES
C!    NSVHX ...... SPACING BETWEEN CONSECUTIVE HELICES
C!    TNU(I) ...... NEUTRAL TRACK PARAMETERS
C!    NSNU ....... SPACING BETWEEN CONSECUTIVE HELICES
C!    VTNU(I) ..... HELIX VARIANCES
C!    NSVNU ...... SPACING BETWEEN CONSECUTIVE HELICES
C!    VTX1 ........ COORD. OF INITIAL VERTEX
C!    VVTX1 ....... VARIANCES  "
C!    CHIV1 ...... CHISQ OF   "
C!    NDFV1 ...... # OF DEGREES OF FREEDOM OF INITIAL VERTEX
C!    MKTV1 ...... TRACK MARKER HAS BIT I SET IF TRACK I IS
C!                  INCLUDED IN INITAL VERTEX
C!    VTX2..MKTV2  SAME FOR FINAL VERTEX
C!    ICONL ...... =1 99% CONFIDENCE LIMIT
C!                  =2 99% CONFIDENCE LIMIT FOR VERTEX
C!    NADTR ...... # OF ADDED TRACKS
C!    IXTRI ....... INDICES OF ADDED TRACKS
C!    CHVXI ...... CHISQ OF INTERMEDIATE VERTICES
C----------------------------------------------------------*
#ifndef DOC
      SAVE
#include "bcs.h"
#include "ydimto.h"
#include "ydumto.h"
C
C     DIMENSION MKTV1(MKDIMM),MKTV2(MKDIMM)
      DIMENSION MKTV1(*),MKTV2(*)
C
      DIMENSION IXHX(*),IXNU(*),VTX1(*),VVTX1(*),VTX2(*),VVTX2(*)
      DIMENSION WTX(3),VWTX(6),WTX2(3),VWTX2(6),CHVXI(*),IXTRI(*)
      DIMENSION MKTRK(MAXTRK)
      DIMENSION CONLM(20,2)
C
      LOGICAL LMRK
C
#if defined(YDEBUG)
      LOGICAL LDMP1,LDMP2
#endif
C
C
      DATA CONLM/ 6.6, 9.2,11.4,13.3,15.1,16.8,18.5,20.1,21.7,23.2,
     +            24.7,26.2,27.7,29.1,30.6,32.0,33.4,34.8,36.2,37.6,
     &            6.6, 9.2,11.4,13.3,15.1,16.8,18.5,20.1,21.7,23.2,
     +            24.7,26.2,27.7,29.1,30.6,32.0,33.4,34.8,36.2,37.6/
C
C
      DATA NENTY/0/
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
#if defined(YDEBUG)
C
      NENTY=NENTY+1
C
      NCPRI=NCPRI+1
      ICPRI=ICPRI+1
C
      IF(IDUMPP(7).GE.1) THEN
        LDMP1=.TRUE.
      ELSE
        LDMP1=.FALSE.
      ENDIF
C
C
      IDUMPP(7)=IDUMPP(7)-1
C
      IF(LDMP1)    THEN
        WRITE(LOUT,9901) NENTY,MKTV1(1),NHX,NEU,
     &    (IXHX(I),I=1,NHX),(IXNU(I),I=1,NEU)
 9901   FORMAT(1X,10(1H*),' YVXBLD CALLED',I5,' TIMES  MKTV1=',Z10,
     &    ' NHX,NEU=',2I4/(5X,' TRACKS',1X,5I3,1X,5I3,1X,5I3,1X,5I3))
        WRITE(LOUT,9902) (VTX1(I),I=1,3),(VVTX1(I),I=1,6),CHIV1,NDFV1,
     &    (MKTV1(I),I=1,MKDIMM)
 9902   FORMAT(5X,'YVXBLD: VTX1=',3E10.3,'  VVTX1=',6E10.3/
     &    14X,' CHIV1=',E10.3,' NDFV1=',I3,' MKTV1=',5(1X,Z8))
      ENDIF
#endif
C
      NADTR=0
C
      DO 100 I=1,3
        WTX(I)=VTX1(I)
        VTX2(I)=VTX1(I)
  100 CONTINUE
      DO 200 I=1,6
        VWTX(I)=VVTX1(I)
        VVTX2(I)=VVTX1(I)
  200 CONTINUE
      CHIWX=CHIV1
      CHVX2=CHIV1
      NDFWX=NDFV1
      NDFV2=NDFV1
C
C     MKTV2=MKTV1
      CALL YMKCOP(MKDIMM,MKTV1(1),MKTV2(1))
C
      NADTR=0
C
      NTRKS=NHX+NEU
      DO ITRK=1,NTRKS
        MKTRK(ITRK)=1
      ENDDO
      DO 2000 IADD=1,NTRKS
C ****======= TEMPORARY CODING
        NDF=NDFV2+2
        IF(NDF.GT.20) GO TO 860
        XILIM=CONLM(NDF,ICONL)
        GO TO 870
  860   CONTINUE
        XILIM=2.25*NDF
        IF(ICONL.EQ.2) XILIM=2.25*NDF
C
  870   CONTINUE
        NMWD0=(NTRKS-1)/NMSIZZ+1
C
        CHMIN=1.E+30
        IBTRK=0
        DO 1000 ITRK=1,NTRKS
          IF(MKTRK(ITRK).EQ.0) GO TO 1000
          IF(ITRK.LE.NHX) THEN
            NH=1
            NU=0
            IXH=IXHX(ITRK)
            JTIX=IXH
            IF(IXH.LE.0) THEN
              MKTRK(ITRK)=0
              GO TO 1000
            ENDIF
          ELSE
            NH=0
            NU=1
            IXN=IXNU(ITRK-NHX)
            JTIX=IXN+MAXHLX
            IF(IXN.LE.0) THEN
              MKTRK(ITRK)=0
              GO TO 1000
            ENDIF
          ENDIF
#if defined(YDEBUG)
          IF(LDMP1)
     &      WRITE(LOUT,9905) IADD,ITRK,NH,NU,IXH,IXN,MKTV2(1)
 9905     FORMAT(' YVXBLD  IADD,ITRK',2I3,' NH,NU',2I3,' IXH,IXN',2I3,
     &      '  MKTV2=',Z10)
#endif
C     TRACK ALREADY IN VERTEX?
          CALL YMKTST(MKDIMM,NMSIZZ,MKTV2(1),JTIX,LMRK)
          IF(.NOT.LMRK) THEN
            CALL YFTVTR(1  ,NH,NU,.FALSE.,WTX,VWTX,
     &        IXH,NSHX,NSVHX,HXI,VHXI,
     &        IXN,NSNU,NSVNU,TNU,VTNU,
     &        WTX2,VWTX2,CHISQ,IFAIL)
#if defined(YDEBUG)
            IF(LDMP1)
     &        WRITE(LOUT,9907) IADD,ITRK,CHISQ,WTX2,SQRT(VWTX2(1)),
     &        SQRT(VWTX2(3)),SQRT(VWTX2(6))
 9907       FORMAT(' YVXBLD:  IADD,ITRK',2I3,' CHISQ=',E10.2,
     &        ' WTX2',3F9.5,2X,3F9.5)
#endif
C     REMOVE VERY FAR OFF TRACKS FROM FURTHER SEARCH
            IF(CHISQ.LE.2.*CONLM(2,ICONL).AND.(CHIWX+CHISQ).LE.XILIM)
     &        GO TO 500
            MKTRK(ITRK)=0
            GO TO 1000
  500       CONTINUE
C
            IF(CHISQ.GT.CONLM(2,ICONL)) GO TO 1000
            IF(CHISQ+CHIWX.GT.XILIM) GO TO 1000
C
            CHNOR=CHISQ
C     GIVE PREFERENCE TO TRACKS WITH GOOD RESOLUTION
CC      CHNOR=CHISQ/TRRES(ITRK,1)**2
            CHNOR=CHISQ
C
            IF(CHNOR.GE.CHMIN) GO TO 1000
C     BETTER TRACK FOUND
            IBTRK=ITRK
            CHBTR=CHISQ
            CHMIN=CHNOR
#if defined(YDEBUG)
            IF(LDMP1)
     &        WRITE(LOUT,550) ITRK,IBTRK,CHBTR,CHMIN
  550       FORMAT('   ITRK,IBTRK,CHBTR,CHMIN',2I5,2E10.2)
#endif
          ENDIF
C
 1000   CONTINUE
        IF(IBTRK.LE.0) GO TO 3000
        IF(IBTRK.LE.NHX) THEN
          NH=1
          NU=0
          IXH=IXHX(IBTRK)
          JTIX=IXH
        ELSE
          NH=0
          NU=1
          IXN=IXNU(IBTRK-NHX)
          JTIX=IXN+MAXHLX
        ENDIF
        CALL YFTVTR(1  ,NH,NU,.FALSE.,WTX,VWTX,
     &    IXH,NSHX,NSVHX,HXI,VHXI,
     &    IXN,NSNU,NSVNU,TNU,VTNU,
     &    VTX2,VVTX2,CHISQ,IFAIL)
        NDFV2=NDFV2+2
        CHVX2=CHIWX+CHISQ
#if defined(YDEBUG)
        IF(LDMP1)
     &    WRITE(LOUT,9811) IBTRK,CHIWX,CHBTR,NDF,XILIM,
     $    (VTX2(I),I=1,3),CHISQ,CHVX2
 9811   FORMAT(' YVXBLD ADD IBTRK=',I3,' CHIWX,CHBTR',2F6.1,
     &    ' NDF,XILIM',I3,F6.1,' VTX2',3F7.2,' CHISQ,CHVX2',2E10.2)
#endif
C
        DO 1100 I=1,3
          WTX(I)=VTX2(I)
 1100   CONTINUE
        DO 1200 I=1,6
          VWTX(I)=VVTX2(I)
 1200   CONTINUE
        NDFWX=NDFV2
        CHIWX=CHVX2
C
        CALL YMKSET(MKDIMM,NMSIZZ,MKTV2(1),JTIX)
C
        MKTRK(IBTRK)=0
C
        NADTR=NADTR+1
        IXTRI(NADTR)=JTIX
        CHVXI(NADTR)=CHVX2
C
 2000 CONTINUE
C
 3000 CONTINUE
C
      RETURN
      END
#endif
