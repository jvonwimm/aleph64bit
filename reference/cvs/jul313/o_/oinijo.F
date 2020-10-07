      SUBROUTINE OINIJO
C----------------------------------------------------------------------
C!  - BOM : Job Initialisation
C!
C!   Author   :- E. Lancon             12-AUG-1991
C!
C!      Set Init. Flags to .FALSE.
C?
C!======================================================================
#ifndef DOC
#include "bomflg.h"
C.......................................................................
C
      XCURFL = .FALSE.
      XINIFL = .FALSE.
      IMCURR = 0
C
  999 RETURN
      END
#endif
