      INTEGER FUNCTION VXYZVU (XYZ,JWAF,VUW)
C ----------------------------------------------------------------------
CKEY VDETDES TRANSFORM / USER
C!  Transform ALEPH coordinates into wafer coordinates
C - Joe Rothberg and Rainer Wallny, 15 January 1994
C
C - Input:
C   XYZ(3) / R  Coordinates of point in ALEPH system
C   JWAF   / I  Global wafer index
C
C - Output:
C   VXYZVU / I  = VDOK if successful
C               = VDERR if error occurred
C   VUW(3) / R  Coordinates of point in wafer system
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
#include "vdetge.h"
C
      REAL VUW(3)
      REAL XYZ(3)
      INTEGER JWAF
C
C     Local variables
      INTEGER JFAC
      INTEGER VJFACW
C
C ----------------------------------------------------------------------
C
C check validity of arguments
C
      IF (JWAF .GE. 1 .AND. JWAF .LE. NWAFER) THEN
C
C ----------------------------------------------------------------------
        JFAC = VJFACW(JWAF)
C
C        undo the translation
C
C        [x']    [x]   [x wafer center]
C        [y']  = [y] - [y wafer center]
C        [z']    [z]   [z wafer center]
C
C        and undo the rotation
C
C        [v]    [ cos(phi)  sin(phi) 0]   [x']
C        [u]  = [-sin(phi)  cos(phi) 0] * [y']
C        [w]    [    0          0    1]   [z']
C
        VUW(1) = CPHIOF(JFAC)*(XYZ(1)-WAXCEN(JWAF))
     &          +SPHIOF(JFAC)*(XYZ(2)-WAYCEN(JWAF))
        VUW(2) =-SPHIOF(JFAC)*(XYZ(1)-WAXCEN(JWAF))
     &          +CPHIOF(JFAC)*(XYZ(2)-WAYCEN(JWAF))
        VUW(3) =  (XYZ(3) -  WAZCEN(JWAF))
C
        VXYZVU = VDOK
C
      ELSE
C
C     argument JWAF out of range
C
        VXYZVU = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
