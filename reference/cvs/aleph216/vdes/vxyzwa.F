      INTEGER FUNCTION VXYZWA (JWAF,XYZ)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns (x,y,z) of wafer center
C - Steve Wasserbaech, January 1995
C
C - Input:
C   JWAF   / I  Global wafer index
C
C - Output:
C   VXYZWA / I  = VDOK if successful;
C               = VDERR if JWAF is invalid.
C   XYZ(3) / R  Coordinates of wafer center (cm) in ALEPH system
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER JWAF
      REAL XYZ(3)
C
C     Local variables
      INTEGER JWF, STATUS
C
C     External references:
      INTEGER VJWAFF
C
C ----------------------------------------------------------------------
C
      IF ((JWAF .LT. 1) .OR. (JWAF .GT. NWAFER)) THEN
C
        VXYZWA = VDERR
        CALL VZERO(XYZ,3)
C
      ELSE
C
        VXYZWA = VDOK
        XYZ(1) = WAXCEN(JWAF)
        XYZ(2) = WAYCEN(JWAF)
        XYZ(3) = WAZCEN(JWAF)
C
      ENDIF
C
      RETURN
      END
#endif
