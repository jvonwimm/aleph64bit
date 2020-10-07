      SUBROUTINE QFCALD
CKEY FILL CAL /INTERNAL
C----------------------------------------------------------------------
C! Fill QVEC with calorimeter objects.
C!
C! called from QFILL
C!                                                   H.Albrecht 27.2.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE INIT,IECAL,NECAL
#include "qcdesh.h"
      DATA INIT /0/
C----------------------------------------------------------------------
C
      IF (INIT .EQ. 0) THEN
        INIT = 1
        IECAL = KPCOMP ('ECAL')
        NECAL = KFPADR (IECAL)
      ENDIF
C
C       Fill QCEC with ECAL and HCAL obj. Leave space for comp. obj:
C
      CALL QFCOBJ
C
C       Last chance to ignore the relations between cal objects :
C
   20 CALL QFCREL
C
C       Combine ECAL and HCAL if associated to each other :
C
      CALL QFCASC
C
      END
#endif
