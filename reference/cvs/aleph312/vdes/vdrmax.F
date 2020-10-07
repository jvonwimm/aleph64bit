      REAL FUNCTION VDRMAX ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns maximum radius of all wafers in VDET
C - Steve Wasserbaech, September 1994
C
C - Input:
C   (none)
C
C - Output:
C   VDRMAX / R  Maximum radius (cm) of all wafers in VDET
C               (furthest point within wafers from z axis)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vdetge.h"
C
C ----------------------------------------------------------------------
C
      VDRMAX = RVDMAX
C
      RETURN
      END
#endif
