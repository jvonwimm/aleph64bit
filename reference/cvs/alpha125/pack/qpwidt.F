      FUNCTION QPWIDT (CPAR)
CKEY PART /USER
C----------------------------------------------------------------------
C! Particle width
C                                                   H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER * (*) CPAR
C
      QPWIDT = RW(KOQPAR+KPART(CPAR)*KCQPAR+JQPAWI)
      END
#endif
