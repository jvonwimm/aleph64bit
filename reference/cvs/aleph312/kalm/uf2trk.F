      SUBROUTINE UF2TRK(TRK_IN, DBL_TRK, *)
C!  Initialize input track and check constraints for Kalman filter
#ifndef DOC
      REAL TRK_IN(5)
      DOUBLE PRECISION OMEGA_TRK,D0_TRK,PHI0_TRK,TANL_TRK,Z0_TRK
      DOUBLE PRECISION DBL_TRK(5)

#include "uftcom.h"

C protect against unphysical tracks

      IF(TRK_IN(1)*TRK_IN(4).GT.1.) RETURN 1

C check curvature

      OMEGA_TRK = DBLE(TRK_IN(1))
      IF ( FIXED_OME ) OMEGA_TRK = DBLE(OME_FIX)
      IF ( OMEGA_TRK .EQ. 0. ) OMEGA_TRK = .0000001D0

C convert track params to DOUBLE

      TANL_TRK = DBLE(TRK_IN(2))
      PHI0_TRK  = DBLE(TRK_IN(3))
      D0_TRK  = DBLE(TRK_IN(4))
      Z0_TRK  = DBLE(TRK_IN(5))

C apply constraints if applicable

      IF ( FIXED_TANL ) TANL_TRK = DBLE(TANL_FIX)
      IF ( FIXED_P ) OMEGA_TRK = DSIGN(1.D0,OMEGA_TRK)
     &  /(DBLE(ROVERP*P_FIX)) *
     +  DSQRT(1.+TANL_TRK*TANL_TRK)

C Load output values

      DBL_TRK(1) = OMEGA_TRK
      DBL_TRK(2) = TANL_TRK
      DBL_TRK(3) = PHI0_TRK
      DBL_TRK(4) = D0_TRK
      DBL_TRK(5) = Z0_TRK

      RETURN
      END
#endif
