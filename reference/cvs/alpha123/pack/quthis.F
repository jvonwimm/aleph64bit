      SUBROUTINE QUTHIS
CKEY TERM HIST /USER
C----------------------------------------------------------------------
C! histogram output
C called from QMTERM
C                                                   H.Albrecht 20.09.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
C--------------------------------------------------------------------
      IF (KUPTER .GT. 0)  WRITE (KUPTER,1001)
      IF (XPRHIS) CALL HISTDO
      IF (XHISTO)  THEN
        CALL QWMESS
     +    ('_QUTHIS_ Write histograms/ntuples on histogram file')
        CALL HROUT (0, ICYCLE, 'T')
        CALL HREND ('ALPHA')
        CLOSE (KUHIST)
      ENDIF
 1001 FORMAT (' _QUTHIS_ Print histograms')
      END
#endif
