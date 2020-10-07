      SUBROUTINE EFIJOB
C----------------------------------------------------------------------
C!  - Initialize  ENERGYFLOW  JOB
C!
C!   Author   :- M.N. Minard
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
      INTEGER MDARD,DUMMY
      DUMMY=0
      IDB = JUNIDB(DUMMY)
      IRE2= MDARD(IW,IDB,'EFZC',0)
      IRE3= MDARD(IW,IDB,'EFTD',0)
      IF(IRE2.EQ.0.OR.IRE3.EQ.0)THEN
          CALL RERROR('EFIJOB',-1,
     +    'Data base banks for Energyflow not found')
      ELSE
         CALL EMSKJI
      ENDIF
      RETURN
      END
#endif
