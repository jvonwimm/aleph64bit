      SUBROUTINE YFIJOB
C----------------------------------------------------------------
C! Initialise name indices and formats bank for main vertex
C!
C!     AUTHOR:  MERMEKIDES ?
C!     MODIFIED:    M.BOSMAN  11/04/89
C!
C!
C?              Description
C?              ===========
C?     Initialise name indices and formats bank for
C?     the M.Mermekides primary vertex reconstruction routine
C?
C!======================================================================
#ifndef DOC
#include "bosext.h"
#include "yynamc.h"
C
C  Initialise name indices and formats
      NAYVXL=NAMIND('YVXL')
      NAYTRL=NAMIND('YTRL')
C
C
      RETURN
      END
#endif
