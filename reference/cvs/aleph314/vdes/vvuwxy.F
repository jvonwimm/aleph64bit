      INTEGER FUNCTION VVUWXY (VUW,JWAF,XYZ)
C ----------------------------------------------------------------------
CKEY VDETDES TRANSFORM / USER
C!  Transform wafer coordinates into ALEPH coordinates
C - Joe Rothberg and Rainer Wallny, 15 January 1994
C
C - Input:
C   VUW(3) / R  Coordinates of point in wafer system
C   JWAF   / I  Global wafer index
C
C - Output:
C   VVUWXY / I  = VDOK if successful
C               = VDERR if error occurred
C   XYZ(3) / R  Coordinates of point in ALEPH system
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vdetge.h"
#include "vgindx.h"
C
      REAL VUW(3)
      REAL XYZ(3)
      INTEGER JWAF
      INTEGER VJFACW
C
C     Local variables
      INTEGER JFAC
C
C ----------------------------------------------------------------------
C
C check validity of arguments
C
      IF (JWAF .GE. 1 .AND. JWAF .LE. NWAFER) THEN
C
        JFAC = VJFACW(JWAF)
C
C        do the rotation
C
C        [x']    [cos(phi) -sin(phi) 0]   [v]
C        [y']  = [sin(phi)  cos(phi) 0] * [u]
C        [z']    [   0          0    1]   [w]
C
C and do the translation
C
C        [x]    [x']   [x wafer center]
C        [y]  = [y'] + [y wafer center]
C        [z]    [z']   [z wafer center]
C
        XYZ(1) = CPHIOF(JFAC)*VUW(1)-SPHIOF(JFAC)*VUW(2)+WAXCEN(JWAF)
        XYZ(2) = SPHIOF(JFAC)*VUW(1)+CPHIOF(JFAC)*VUW(2)+WAYCEN(JWAF)
        XYZ(3) = VUW(3)+WAZCEN(JWAF)
C
        VVUWXY = VDOK
C
      ELSE
C
C     argument JWAF out of range
C
        CALL VZERO(XYZ,3)
        VVUWXY = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
