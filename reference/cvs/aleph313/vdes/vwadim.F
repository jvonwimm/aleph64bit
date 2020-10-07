      INTEGER FUNCTION VWADIM (DVUW)
C ----------------------------------------------------------------------
CKEY VDETDES WAFER / USER
C!  Returns the external dimensions of a wafer
C - Steve Wasserbaech, February 1994
C
C - Input:
C   (none)
C
C - Output:
C   VWADIM  / I  = VDOK if successful;
C                = VDERR if an error occurred.
C   DVUW(3) / R  Dimensions of wafer (cm) along v, u, and w directions
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vwgeco.h"
C
C     Arguments:
      REAL DVUW(3)
C
C ----------------------------------------------------------------------
C
      DVUW(1) = WTHICK
      DVUW(2) = WSIZEB
      DVUW(3) = WSIZEA
C
      VWADIM = VDOK
C
      RETURN
      END
#endif
