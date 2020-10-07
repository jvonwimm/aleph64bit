      SUBROUTINE QFECWM
CKEY FILL ECAL /INTERNAL
C----------------------------------------------------------------------
C!  - Fill ECAL wire energies from MDST
C!
C!   Author   :- E. Blucher     27-AUG-1990
C!   Description
C!   ===========
C! Fills QEECWI(IMOD) from DEWI bank.
C!   ===> called from QFILL
C!======================================================================
#ifndef DOC
#include "qcdesh.h"
#include "bmacro.h"
      KDEWI = IW(NADEWI)
      IF(KDEWI.NE.0)THEN
        DO 10 I=1,LROWS(KDEWI)
          QEECWI(ITABL(KDEWI,I,1))=FLOAT(ITABL(KDEWI,I,2))/1000.
10      CONTINUE
      ENDIF
  999 RETURN
      END
#endif
