*DK adinit
      SUBROUTINE ADINIT
C------------------------------------------------------------------
C B. BLOCH-DEVAUX  - 850910
C!  Initialises capture file and set up view banks
C
C   Called by user   in ASIPAC   from this .HLB
C   Calls HPLCAP
C   Calls GUINTI,ADXY,ADRZ   from this .HLB
C ---------------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
      CALL HPLCAP(-10)
      CALL GUINTI
      CALL ADXY(1)
      CALL ADRZ(2)
      RETURN
      END
#endif
