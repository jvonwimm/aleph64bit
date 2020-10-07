      FUNCTION QPLIFE (CPAR)
CKEY PART /USER
C----------------------------------------------------------------------
C! Particle life time
C                                                   H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER * (*) CPAR
C
      QPLIFE = RW(KOQPAR+KPART(CPAR)*KCQPAR+JQPALT)
      END
#endif
