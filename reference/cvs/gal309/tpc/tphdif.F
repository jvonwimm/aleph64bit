*DK tphdif
      FUNCTION TPHDIF(PHI1,PHI2)
C
C  Returns difference between PHI1 and PHI2 in range -pi to + pi
C  (PHI1, PHI2 are in range 0 - 2pi)
C
C ---------------------------------------------------------
#ifndef DOC
#include "alcons.h"
C
      DPHI = PHI1 - PHI2
      IF(DPHI.GT. PI) DPHI =-TWOPI + DPHI
      IF(DPHI.LT.-PI) DPHI = TWOPI + DPHI
      TPHDIF = DPHI
      RETURN
      END
#endif
