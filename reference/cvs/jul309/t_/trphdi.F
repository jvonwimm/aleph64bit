      FUNCTION TRPHDI(PHI1,PHI2)
C
C! Difference between PHI1 ind PHI2 in range -pi to + pi
C  (PHI1, PHI2 are in range 0 - 2pi)
C
#ifndef DOC
#include "alcons.h"
C
      DPHI = PHI1 - PHI2
      IF(DPHI.GT. PI) DPHI =-TWOPI + DPHI
      IF(DPHI.LT.-PI) DPHI = TWOPI + DPHI
      TRPHDI = DPHI
      RETURN
      END
#endif
