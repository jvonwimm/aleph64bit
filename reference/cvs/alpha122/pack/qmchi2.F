      FUNCTION QMCHI2 (ITK, CPAR)
CKEY PART /USER
C----------------------------------------------------------------------
C! Chi**2 from mass difference
C                                                   H.Albrecht 11.01.90
C   Bug fixed                                       J.Boucrot  01.11.94
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      CHARACTER * (*) CPAR
      REAL EPSIL / 1.E-10 /
#include "qmacro.h"
C
      SIGMA = QSIGMM(ITK)
      IF (SIGMA.LT.EPSIL) THEN
         QMCHI2=1./EPSIL
      ELSE
         QMCHI2 = (QM(ITK) - RW(KOQPAR+KPART(CPAR)*KCQPAR+JQPAMA))**2 /
     &            SIGMA
      ENDIF
      END
#endif
