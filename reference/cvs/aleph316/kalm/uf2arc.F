      SUBROUTINE UF2ARC(INNER, OUTER, DBL_TRK, R_COORD,
     &  X_SMOOTH, XY_ARC)
C!  Calculate arc-length along track to reach coordinates
#ifndef DOC
      INTEGER INNER, OUTER
      DOUBLE PRECISION R_COORD(*), X_SMOOTH(5,*), XY_ARC(*)

      DOUBLE PRECISION OMEGA_TRK,D0_TRK,PHI0_TRK,TANL_TRK,Z0_TRK
      DOUBLE PRECISION DBL_TRK(5)

#include "uftcom.h"

      DOUBLE PRECISION RADIUS, CS, SS, RSFI, RCFI
      DOUBLE PRECISION EPS
      PARAMETER (EPS = 1.0D-15)

C  Load local variables

      OMEGA_TRK = DBL_TRK(1)
      TANL_TRK = DBL_TRK(2)
      PHI0_TRK = DBL_TRK(3)
      D0_TRK = DBL_TRK(4)
      Z0_TRK = DBL_TRK(5)

C  Use preliminary fit for initial smoothed state vector
C  The jacobians will be computed at these positions

      CS  = DCOS(PHI0_TRK)*(1.D0-D0_TRK*OMEGA_TRK)
      SS  = DSIN(PHI0_TRK)*(1.D0-D0_TRK*OMEGA_TRK)
      DO I = OUTER, INNER
        RADIUS = R_COORD(I)
        XY_ARC(I)    = 2.D0/OMEGA_TRK*DASIN( 0.5D0*OMEGA_TRK*
     +     DSQRT(DABS((RADIUS**2-D0_TRK**2)/(1.D0-OMEGA_TRK*D0_TRK))) )
        X_SMOOTH(2,I) = Z0_TRK  + XY_ARC(I)*TANL_TRK
        X_SMOOTH(3,I) = PHI0_TRK  + OMEGA_TRK*XY_ARC(I)
        CALL UBA2PI(X_SMOOTH(3,I))
        X_SMOOTH(4,I) = DATAN( TANL_TRK )
        X_SMOOTH(5,I) = OMEGA_TRK
        RSFI = - DCOS(PHI0_TRK+OMEGA_TRK*XY_ARC(I)) + CS
        RCFI = + DSIN(PHI0_TRK+OMEGA_TRK*XY_ARC(I)) - SS +
     +                                  RADIUS*OMEGA_TRK
        IF (DABS(RCFI).LT.EPS) RCFI = DSIGN(EPS, RCFI)
        X_SMOOTH(1,I) = 2.D0 * DATAN2( RSFI, RCFI )
        CALL UBA2PI(X_SMOOTH(1,I))
        X_SMOOTH(1,I) = RADIUS * X_SMOOTH(1,I)
      ENDDO

      RETURN
      END

#endif
