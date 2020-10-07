      INTEGER FUNCTION VFADIM (DVUW)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns dimensions of a face
C - Steve Wasserbaech, February 1994
C
C - Input:
C   (none)
C
C - Output:
C   VFADIM  / I  = VDOK if successful;
C                = VDERR if an error occurred
C
C   DVUW(3) / R  Dimensions of face (cm) along local
C                v, u, and w directions
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vzpwco.h"
#include "vwgeco.h"
C
C     Arguments:
      REAL DVUW(3)
C
C ----------------------------------------------------------------------
C
C     Along v and u, we simply need the wafer dimensions:
C
      DVUW(1) = WTHICK
      DVUW(2) = WSIZEB
C
C     Along w, we need the wafer dimension plus the distance
C     between the centers of the first and last wafers in the face:
C
      DVUW(3) = WSIZEA + (WAFERZ(NWAFEF) - WAFERZ(1))
C
      VFADIM = VDOK
C
      RETURN
      END
#endif
