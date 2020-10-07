      INTEGER FUNCTION UFGETT(RADIUS, STATE, T_PROC)
C!  Calculate the process time at a given radius
#ifndef DOC
      DOUBLE PRECISION RADIUS, STATE(*), T_PROC
      DOUBLE PRECISION DELTA_PHI, DENOM, NUMER
      DOUBLE PRECISION EPS
      PARAMETER (EPS = 1.0D-15)
#include "uftcom.h"

      UFGETT = -1

      IF (ABS(STATE(5)).LE. EPS) RETURN
      DELTA_PHI = STATE(3) - STATE(1)/RADIUS
      DENOM = RADIUS * DCOS(DELTA_PHI)
      NUMER = (STATE(5))**(-1) - RADIUS*DSIN(DELTA_PHI)
      IF ( ABS(NUMER) .LT. EPS ) THEN
        IF ( ABS(DENOM) .LT. EPS ) RETURN
        T_PROC = DATAN( NUMER / DENOM ) - HALF_PI
      ELSE
        T_PROC = - DATAN( DENOM / NUMER )
      ENDIF
      IF ( T_PROC .GT. 0.D0 .AND. STATE(5) .GT. 0.D0 )
     +                                          T_PROC = T_PROC - ONE_PI
      IF ( T_PROC .LT. 0.D0 .AND. STATE(5) .LT. 0.D0 )
     +                                          T_PROC = T_PROC + ONE_PI

      UFGETT = 0

      RETURN
      END
#endif
