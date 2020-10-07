      REAL FUNCTION VDZMAX ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns maximum abs(z) of all wafers in VDET
C - Steve Wasserbaech, September 1994
C
C - Input:
C   (none)
C
C - Output:
C   VDZMAX / R  Maximum abs(z) (cm) of all wafers in VDET
C               (furthest point within wafers from xy plane)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vdetge.h"
C
C ----------------------------------------------------------------------
C
      VDZMAX = ZVDMAX
C
      RETURN
      END
#endif
