      INTEGER FUNCTION VXYZFC (JFAC,XYZ)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns (x,y,z) of face center
C - Steve Wasserbaech, February 1994
C
C - Input:
C   JFAC   / I  Global face index
C
C - Output:
C   VXYZFC / I  = VDOK if successful;
C               = VDERR if JFAC is invalid.
C   XYZ(3) / R  Coordinates of face center (cm) in ALEPH system;
C               the "face" consists of the NWAFEF wafers
C               and the spaces in between them.
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vzpwco.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER JFAC
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
      IF ((JFAC .LT. 1) .OR. (JFAC .GT. NSLOTS)) THEN
C
        VXYZFC = VDERR
        CALL VZERO(XYZ,3)
C
      ELSE
C
        VXYZFC = VDOK
C
C     Get the global index of the first wafer in the face:
C
        STATUS = VJWAFF(JFAC,1,JWF)
C
C     All wafers in a face have the same (x,y), so we can get
C     the first two components directly from WAXCEN and WAYCEN:
        XYZ(1) = WAXCEN(JWF)
        XYZ(2) = WAYCEN(JWF)
C
C     The face center lies midway between the first and last wafers:
C
        XYZ(3) = (WAFERZ(1) + WAFERZ(NWAFEF)) / 2.
C
      ENDIF
C
      RETURN
      END
#endif
