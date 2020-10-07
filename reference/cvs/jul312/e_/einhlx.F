      SUBROUTINE EINHLX(STEP,IFILD,X)
C***********************************************************************
C! INITIALIZE HELIX PARAMETERS                                         *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  861108                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C!    INPUT :                                                          *
C!               STEP  = STEP SIZE                                     *
C!               X(8)  = LAST TPC POINT (LATER,CURRENT POINT)          *
C!                                                                     *
C!    OUTPUT : NONE                                                    *
C!                                                                     *
C***********************************************************************
#ifndef DOC
C
      DOUBLE PRECISION RAD,CFILD,STPEH,PXY
      DIMENSION X(*)
#include "ehlcom.h"
#include "ectcom.h"
C
C?INITIALISE STEP OF TRACKING
C?
      STPEH = DBLE(STEP)
      CFILD=DBLE(FILDEC*FLOAT(IFILD))
      DO 20 I=1,8
   20 POINEH(I)=DBLE(X(I))
C
C?   IF CHARGED TRACK ...
C
      IF(POINEH(8)*CFILD.NE.0.D0)THEN
        PXY=DSQRT(POINEH(4)**2+POINEH(5)**2)
        RAD=POINEH(7)/(DBLE(AAAKEC)*POINEH(8)*DBLE(CFILD))*PXY
        THRES=0.1*ABS(SNGL(RAD))
        IF(THRES.LT.STPEH)STPEH=DBLE(THRES)
      SINUEH=DSIN(STPEH*PXY/RAD)
      STXYEH=RAD*SINUEH/PXY
      STZZEH=STPEH
        COSIEH=DSQRT(1.D0-SINUEH**2)
        CONSEH=(1.D0-COSIEH)/SINUEH
      ELSE
        CONSEH=0.D0
        COSIEH=1.D0
        SINUEH=0.D0
      STXYEH=STPEH
      STZZEH=STPEH
      ENDIF
  999 RETURN
      END
#endif
