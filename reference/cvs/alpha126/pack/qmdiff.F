      FUNCTION QMDIFF (ITK, CPAR)
CKEY PART /USER
C----------------------------------------------------------------------
C! Mass difference
C                                                   H.Albrecht 11.01.90
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER * (*) CPAR
C
      QMDIFF = RW(KOQVEC+ITK*KCQVEC+JQVEQM) -
     &         RW(KOQPAR+KPART(CPAR)*KCQPAR+JQPAMA)
      END
#endif
