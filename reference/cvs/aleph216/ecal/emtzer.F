      REAL FUNCTION EMTZER ( IMODU )
C -------------------------------------------------------------------
C! return tzero of an ECAL Module # IMODU
C  return -9999. if PEWI/PWEI does exist or if IMODU is not there
CKEY ECAL T0
C - M.N.Minard - 950510
C ------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "bmacro.h"
C ----------------------------------------------------------
      EMTZER = -9999.
      KPEWI = NLINK ( 'PEWI',0)
      IF (KPEWI.EQ.0) KPEWI = NLINK('PWEI',0)
      IF (KPEWI.GT.0) THEN
        LCOL = LCOLS(KPEWI)
        DO IPEWI = 1,LROWS(KPEWI)
          IF ( ITABL(KPEWI,IPEWI,1).EQ.IMODU) THEN
             EMTZER = REAL (ITABL (KPEWI,IPEWI,LCOL))
          ENDIF
        ENDDO
      ENDIF
      END
#endif
