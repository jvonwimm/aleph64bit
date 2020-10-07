      INTEGER FUNCTION VNTWAF ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Total number of wafers
C - Joe Rothberg, December 1993
C
C - Input:
C   (none)
C
C - Output:
C   VNTWAF / I  Total number of wafers in VDET
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
C ----------------------------------------------------------------------
C
      VNTWAF = NWAFER
C
      RETURN
      END
#endif
