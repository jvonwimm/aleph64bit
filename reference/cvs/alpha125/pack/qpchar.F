      FUNCTION QPCHAR (CPAR)
CKEY PART /USER
C----------------------------------------------------------------------
C! Particle mass
C                                                   H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER * (*) CPAR
C
      QPCHAR = RW(KOQPAR+KPART(CPAR)*KCQPAR+JQPACH)
      END
#endif
