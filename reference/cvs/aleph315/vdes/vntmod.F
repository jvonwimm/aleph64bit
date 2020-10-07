      INTEGER FUNCTION VNTMOD ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Total number of modules
C - Joe Rothberg, December 1993
C
C - Input:
C   (none)
C
C - Output:
C   VNTMOD / I  Total number of modules in VDET
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
C ----------------------------------------------------------------------
C
      VNTMOD = NMODUL
C
      RETURN
      END
#endif
