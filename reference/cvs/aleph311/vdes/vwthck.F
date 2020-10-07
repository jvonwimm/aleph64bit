      REAL FUNCTION VWTHCK ()
C ----------------------------------------------------------------------
CKEY VDETDES WAFER / USER
C!  Returns the thickness of the wafers
C - Steve Wasserbaech, September 1994
C
C - Input:
C   (none)
C
C - Output:
C   VWTHCK / R  Thickness (cm) of wafer
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vwgeco.h"
C
C ----------------------------------------------------------------------
C
      VWTHCK = WTHICK
C
      RETURN
      END
#endif
