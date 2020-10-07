      REAL FUNCTION VDRMIN ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns minimum radius of all wafers in VDET
C - Steve Wasserbaech, September 1994
C
C - Input:
C   (none)
C
C - Output:
C   VDRMIN / R  Minimum radius (cm) of all wafers in VDET
C               (nearest point within wafers to z axis)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vdetge.h"
C
C ----------------------------------------------------------------------
C
      VDRMIN = RVDMIN
C
      RETURN
      END
#endif
