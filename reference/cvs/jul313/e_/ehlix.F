      SUBROUTINE EHLIX(X)
C
C***********************************************************************
C!GET NEXT POINT ON HELIX                                              *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  860615                                    *
C!  MODIFIED : A. BONISSENT  861108                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!    SUBROUTINE EHLIX                                                 *
C!      X(8) = NEXT POINT ON HELIX X,Y,Z,U,V,W,P,CHARGE                *
C!                                                                     *
C***********************************************************************
C
#ifndef DOC
#include "bcs.h"
#include "ehlcom.h"
      DOUBLE PRECISION P4SIN,P4COS,P5SIN,P5COS
      DIMENSION X(*)
      POINEH(1)=POINEH(1)+STXYEH*(POINEH(4)+POINEH(5)*CONSEH)
      POINEH(2)=POINEH(2)+STXYEH*(POINEH(5)-POINEH(4)*CONSEH)
      POINEH(3)=POINEH(3)+STZZEH*POINEH(6)
      P4COS=POINEH(4)*COSIEH
      P4SIN=POINEH(4)*SINUEH
      P5COS=POINEH(5)*COSIEH
      P5SIN=POINEH(5)*SINUEH
      POINEH(4)=P4COS+P5SIN
      POINEH(5)=P5COS-P4SIN
      DO 1 I=1,6
    1 X(I)=SNGL(POINEH(I))
      RETURN
      END
#endif
