      INTEGER FUNCTION VNTFAC ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Total number of slots
C - Joe Rothberg, December 1993
C
C - Input:
C   (none)
C
C - Output:
C   VNTFAC / I  Number of slots in VDET
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
C
C ----------------------------------------------------------------------
C
      VNTFAC = NSLOTS
C
      RETURN
      END
#endif
