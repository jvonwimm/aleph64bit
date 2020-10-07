      SUBROUTINE TATWIN(NBADC,ICBAD,ITPCO,RTPCO,ICH1,ICH2)
C
C----------------------------------------------------------------------
C! Add twin coordinates to track candidates
C
C  Author:  R. Johnson   10-09-89
C
C  Input:   NBADC           /I      Number of bad coordinates
C           ICBAD( )        /I      List of bad coordinates
C           ITPCO( ,2)      /I      The two coordinates
C           RTPCO( ,2)      /R      Same as ITPCO
C           ICH(2)          /I      Pointers to the two chains
C  Output:  ITPCO( ,2)      /I      Add reference to the tracks
C
C?
C!======================================================================
#ifndef DOC
C
#include "tpgpar.h"
#include "tpcojj.h"
#include "tchajj.h"
#include "tcaljj.h"
#include "alcons.h"
#include "bcs.h"
C
      DIMENSION ICBAD(NBADC),ITPCO(LTPCOA,*),RTPCO(LTPCOA,*),ICH(2)
      DIMENSION LIST(LTPDRO),VV0(5),EE0(5),HP(5,2)
      DIMENSION S(2),PHIP(2),ZP(2),PHCH(2),ITKCH(2),ICCH(2)
      DIMENSION RS1CH(2),RS2CH(2),IOTHR(2)
      LOGICAL FIRST
      DATA FIRST/.TRUE./,IOTHR/2,1/
      DATA RSIGMX/16./     !r*phi window in sigma**2 for coordinates
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTCHA=NAMIND('TCHA')
        NTCTC=NAMIND('TCTC')
        NTPCO=NAMIND('TPCO')
        NTCAL=NAMIND('TCAL')
        NTTCC=NAMIND('TTCC')
      ENDIF
      KTCHA=IW(NTCHA)
      KTCTC=IW(NTCTC)
      KTPCO=IW(NTPCO)
      IF (KTCHA.EQ.0 .OR. KTCTC.EQ.0 .OR. KTPCO.EQ.0) THEN
        CALL RERROR('TATWIN',1,'Missing TCHA, TCTC, or TPCO on input')
        GO TO 999
      ENDIF
      KTCAL=IW(NTCAL)
      KTTCC=IW(NTTCC)
      IF (KTCAL.EQ.0 .OR. KTTCC.EQ.0) THEN
        CALL RERROR('TATWIN',2,'Missing TCAL or TTCC on input')
        GO TO 999
      ENDIF
C
C++   Refit the chains if any points were removed
C
      ICH(1)=ICH1
      ICH(2)=ICH2
      DO 100 K=1,2
        ITKCH(K)=0
C
C++     Figure out which track candidate this chain is on.  If the
C++     chain is not on the first loop of a track, ignore it.
C
        IOFF=KTCTC+LMHLEN+ITABL(KTCHA,ICH(K),JTCHOF)
        DO 31 II=1,ITABL(KTCHA,ICH(K),JTCHNC)
          IC=IW(IOFF+II)
          ITK=ITABL(KTPCO,IC,JTPCTN)
          IF (ITK.GT.0) GO TO 32
   31   CONTINUE
C
C++     This chain is not on first loop of a candidate.  Skip it.
C
        GO TO 100
   32   CONTINUE
C
C++     Build a list of remaining hits for this track
C
        NFT=0
        IOFF=KTTCC+LMHLEN+ITABL(KTCAL,ITK,JTCAIO)
        DO 60 II=1,ITABL(KTCAL,ITK,JTCANF)
          IC=IW(IOFF+II)
          DO 40 I=1,NBADC
            IF (IC.EQ.ICBAD(I)) GO TO 60
   40     CONTINUE
          NFT=NFT+1
          LIST(NFT)=IC
   60   CONTINUE
        IF (NFT.LT.3) THEN
          CALL RERROR('TATWIN',1,'Not enough good coordinates'//
     &                ' left to refit track.')
          GO TO 100
        ENDIF
C
C++     Fast helix fit without error matrix, in case coordinates were
C++     dropped since the chain was fit.
C
        IF (NFT.EQ.ITABL(KTCAL,ITK,JTCANF)
     &          .AND. NFT.EQ.ITABL(KTCHA,ICH(K),JTCHNC)) THEN
          VV0(1)=RTABL(KTCHA,ICH(K),JTCHIR)
          VV0(2)=RTABL(KTCHA,ICH(K),JTCHTL)
          VV0(3)=RTABL(KTCHA,ICH(K),JTCHP0)
          VV0(4)=RTABL(KTCHA,ICH(K),JTCHD0)
          VV0(5)=RTABL(KTCHA,ICH(K),JTCHZ0)
        ELSE
          CALL TFTHEL('TPCO',NFT,LIST,1,VV0,EE0,CHI2)
        ENDIF
C
C++     Convert helix parameters to the standard convention
C
        HP(1,K)=-VV0(1)
        HP(2,K)= VV0(2)
        HP(3,K)= VV0(3)
        HP(4,K)= SIGN(1.0,HP(1,K))*VV0(4)
        HP(5,K)= VV0(5)
C
C++     Find intersection with radius of the coordinates
C
        RAD=RTPCO(JTPCRV,1)
        CALL THLCIR(HP(1,K),RAD,S,PHIP,ZP,IER)
        IF (IER.NE.0) GO TO 100
C
C++     Find the closest coordinate in r*phi
C
        RSMN=9.9E20
        RSMX=-1.
        DO 80 I=1,2
          DPHI=PHIP(1)-RTPCO(JTPCPH,I)
          IF (DPHI.LT.-PI) THEN
            DPHI=DPHI+TWOPI
          ELSEIF (DPHI.GT.PI) THEN
            DPHI=DPHI-TWOPI
          ENDIF
          DRPHI=DPHI*RAD
          RSIG=DRPHI**2/RTPCO(JTPCSR,I)
          IF (RSIG.GT.RSMX) THEN
            RSMX=RSIG
          ENDIF
          IF (RSIG.LT.RSMN) THEN
            RSMN=RSIG
            ICBST=I
          ENDIF
   80   CONTINUE
        IF (ABS(RSMN).GT.RSIGMX) GO TO 100
C
        PHCH(K)=PHIP(1)
        ITKCH(K)=ITK
        ICCH(K)=ICBST
        RS1CH(K)=RSMN
        RS2CH(K)=RSMX
  100 CONTINUE
C
C++   Try to associate these new coordinates with the two chains
C
      IF (ITKCH(1).EQ.0 .AND. ITKCH(2).EQ.0) GO TO 999
C
      IF (ITKCH(1).EQ.0 .AND. ITKCH(2).NE.0) THEN
        ITPCO(JTPCTN,ICCH(2))=ITKCH(2)
      ELSEIF (ITKCH(1).NE.0 .AND. ITKCH(2).EQ.0) THEN
        ITPCO(JTPCTN,ICCH(1))=ITKCH(1)
      ELSEIF (ICCH(1).NE.ICCH(2)) THEN
        ITPCO(JTPCTN,ICCH(1))=ITKCH(1)
        ITPCO(JTPCTN,ICCH(2))=ITKCH(2)
      ELSE
C
C++     Both tracks want the same coordinate.  Give it to the
C++     one which is closer.
C
        IF (RS1CH(1).LT.RS1CH(2)) THEN
          ITPCO(JTPCTN,ICCH(1))=ITKCH(1)
          IF (ABS(RS2CH(2)).LT.RSIGMX) THEN
            ITPCO(JTPCTN,IOTHR(ICCH(1)))=ITKCH(2)
          ENDIF
        ELSE
          ITPCO(JTPCTN,ICCH(2))=ITKCH(2)
          IF (ABS(RS2CH(1)).LT.RSIGMX) THEN
            ITPCO(JTPCTN,IOTHR(ICCH(2)))=ITKCH(1)
          ENDIF
        ENDIF
      ENDIF
  999 CONTINUE
      RETURN
      END
#endif
