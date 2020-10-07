      SUBROUTINE EMSKEI
C=======================================================================
C!    Event initialization
C-
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C-
C?
C!======================================================================
#ifndef DOC
#include "pvectr.h"
#include "pjetef.h"
C
C-   Initialize PJETA
C
       CALL VZERO ( PJETA,220)
       CALL VZERO(VRECNF,10)
       CALL VZERO(NFITJT,2)
       CALL VZERO(THRTEF,24)
       ETRACK = 0.
C
       RETURN
       END
#endif
