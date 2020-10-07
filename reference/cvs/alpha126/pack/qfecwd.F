      SUBROUTINE QFECWD
CKEY FILL ECAL /INTERNAL
C----------------------------------------------------------------------
C!  - Fill ECAL wire energies from DST or POT
C!
C!   Author   :- E. Blucher     27-AUG-1990
C!   Description
C!   ===========
C! Fills QEECWI(IMOD) from PEWI bank.
C!   ===> called from QFILL
C!======================================================================
#ifndef DOC
#include "qcdesh.h"
#include "bmacro.h"
      KPEWI = IW(NAPEWI)
      IF ( KPEWI.EQ.0) KPEWI = NLINK('PWEI',0)
      IF(KPEWI.NE.0)THEN
C
        DO 20 IROW = 1, LROWS(KPEWI)
          IMOD = ITABL(KPEWI,IROW,1)
          SUM=0.
          DO 10 I=2,46
            PLANE = FLOAT(ITABL(KPEWI, IROW, I))
            SUM = SUM + PLANE/1000000.
  10      CONTINUE
          QEECWI(IMOD)=SUM
  20    CONTINUE
      ENDIF
  999 RETURN
      END
#endif
