      INTEGER FUNCTION VNRWAF ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Number of wafers in module
C - Joe Rothberg, December 1993
C
C - Input:
C   (none)
C
C - Output:
C   VNRWAF / I  Number of wafers per module
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
C ----------------------------------------------------------------------
      VNRWAF = NWAFEM
C
      RETURN
      END
#endif
