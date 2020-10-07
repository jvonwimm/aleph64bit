      SUBROUTINE EMSKJI
C=======================================================================
C!    Job initialization
C-
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
       DIMENSION XSM ( 7)
C
C-   Initialization routine
C
C-   Define mask constants
C
      CALL EMSKI
C
C-   Initialize pointers
C
      CALL ENFMSK
C
C-   Initialize jet finding
C
       CALL AINJET
       RETURN
       END
#endif
