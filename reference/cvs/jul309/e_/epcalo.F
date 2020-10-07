      SUBROUTINE EPCALO
C***********************************************************************
C! PROPAGATE TRACKS TO ECAL      ENTRY                                 *
C!                                                                     *
C!  AUTHOR   : A. EALET  870224                                        *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ETP1                                                   *
C!    OUTPUT  : ETP1                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C!    INPUT :                                                          *
C!               STEP  = STEP SIZE                                     *
C!               X(8)  = LAST TPC POINT (LATER,CURRENT POINT)          *
C!                                                                     *
C!    OUTPUT :                                                         *
C!               X(8)  =ECAL ENTRY POINT                               *
C***********************************************************************
#ifndef DOC
C
#include "bcs.h"
      DOUBLE PRECISION RAD,CFILD
      DIMENSION X(8)
#include "ehlcom.h"
#include "alcons.h"
#include "ectcom.h"
#include "etp1jj.h"
#include "ecnamc.h"
      DATA RECMN/186.2/,ZECMN/254.2/
#include "bmacro.h"
C
C? LOOP ON TRACKS
C
C
      KETP1=IW(NAETP1)
      NTRAK=LROWS(KETP1)
      DO 10 ITRAK=1,NTRAK
      IREF=KROW(KETP1,ITRAK)
C?
C? CHOOSE ONLY TRACKS WHICH CAN BE PROPAGATED
C? FIND LAST TPC POINT
C?
      IF(IW(IREF+JETPEF).EQ.1)THEN
        DO 11 I=1,3
        X(I)=RW(IREF+JETPX3+I-1)
        X(I+3)=RW(IREF+JETPP3+I-1)
   11   CONTINUE
        X(7)=RW(IREF+JETPPT)
        X(8)=RW(IREF+JETPCH)
C
        CFILD=DBLE(FILDEC)
        DO 30 I=1,8
   30   POINEH(I)=DBLE(X(I))
C
C?   IF CHARGED TRACK ...
C?  COMPUTE THE INTERSECTION WITH BARREL IN XY PLANE
C?    S1 IS THE CURVED ABSCISSA TO THIS POINT
C
        IF(POINEH(8).NE.0.D0)THEN
           PXY=DSQRT(POINEH(4)**2+POINEH(5)**2)
           RAD=POINEH(7)/(DBLE(AAAKEC)*POINEH(8)*DBLE(CFILD))
           XC=POINEH(1)+RAD*POINEH(5)
           YC=POINEH(2)-RAD*POINEH(4)
           RAD=RAD*DBLE(PXY)
           DIS2=XC**2+YC**2
           DIS=SQRT(DIS2)
           CTHET=(SNGL(RAD)**2+DIS2-RECMN**2)/(2.*ABS(SNGL(RAD))*DIS)
           IF(ABS(CTHET).GT.1) THEN
              S1=100000.
              GO TO 20
           ENDIF
           THET=ACOS(CTHET)
           ALPH=-SIGN(1.,SNGL(POINEH(8)))*THET-
     +     (ATAN2((SNGL(POINEH(2))-YC),(SNGL(POINEH(1))-XC))
     +     -ATAN2(-YC,-XC))
      IF(ALPH.GT.TWOPI)ALPH=ALPH-TWOPI
      IF(ALPH.LT.-TWOPI)ALPH=ALPH+TWOPI
           S1=-SNGL(RAD)*ALPH/PXY
C
C? COMPUTE INTERSECTION WITH THE ENDCAP
C
C
   20      S2=(ZECMN-SIGN(1.,SNGL(POINEH(6)))*SNGL(POINEH(3)))/
     +     ABS(SNGL(POINEH(6)))
C?
C? CHOOSE ENDCAP OR BARREL AND COMPUTE THE HELIX STEP IN XY PLANE
C?
           S=AMIN1(S1,S2)
           STEP=S*PXY
C
C? COMPUTE THE NEW ENTRY POINT IN THE CALORIMETER;
C? FIRST, INITIALISE HELIX
C
           DPHI=STEP/SNGL(RAD)
C
C?   IF DISTANCE LESS THAN ONE TURN, TAKE NEW POINT
C
           IF(DPHI.LT.TWOPI)THEN
              SINUEH=SIN(DPHI)
              COSIEH=DSQRT(1.D0-SINUEH**2)
              CONSEH=(1.D0-COSIEH)/SINUEH
              STXYEH=SNGL(RAD*SINUEH)/PXY
              STZZEH=S
C
C?  THEN, PROPAGATE, AND FILL BANK WITH NEW POINT
C
              CALL EHLIX(X)
              DO 12 I=1,3
                RW(IREF+JETPX3+I-1)=X(I)
                RW(IREF+JETPP3+I-1)=X(I+3)
   12         CONTINUE
C
C?   ELSE, SAY THAT TRACK SHOULD NOT BE EXTRAPOLATED
C
          ELSE
              RW(IREF+JETPEF)=0
          ENDIF
        ENDIF
      ENDIF
   10 CONTINUE
  999 RETURN
      END
#endif
