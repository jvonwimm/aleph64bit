      FUNCTION QPMASS (CPAR)
CKEY PART /USER
C----------------------------------------------------------------------
C! Particle mass
C                                                   H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER * (*) CPAR
C
      QPMASS = RW(KOQPAR+KPART(CPAR)*KCQPAR+JQPAMA)
      END
#endif
