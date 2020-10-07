      SUBROUTINE YVIJOB
C----------------------------------------------------------------
C! Initialise the number of v0 for event
C!
C!     AUTHOR:  M.A.CIOCCI
C!     MODIFIED:    M.BOSMAN  11/04/89
C!     Modified: M.A. CIOCCI 16/4/89
C!     Modified: M.A. CIOCCI 6/6/90
C!
C?              Description
C?              ===========
C?     Initialise name indices and formats bank for
C?     the V0 reconstruction routine
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "bosext.h"
#include "y0josu.h"
C   Initialise NV0FOU=Number of V0 found
C              NEVV0F=Number of event with V0
      NV0FOU=0
      NEVV0F=0
      RETURN
      END
#endif
