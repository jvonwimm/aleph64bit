      INTEGER FUNCTION VNLAYR ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Total number of layers
C - Joe Rothberg, December 1993
C
C - Input:
C   (none)
C
C - Output:
C   VNLAYR / I  Number of layers in VDET
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
C
C ----------------------------------------------------------------------
C
      VNLAYR = NVLAYR
C
      RETURN
      END
#endif
