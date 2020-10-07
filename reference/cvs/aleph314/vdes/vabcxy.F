      INTEGER FUNCTION VABCXY (ABC,JWAF,XYZ)
C ----------------------------------------------------------------------
CKEY VDETDES TRANSFORM / USER
C!  Transform local wafer coords (a,b,c) into ALEPH coords (x,y,z)
C - Steve Wasserbaech, 14 October 1994
C
C - Input:
C   ABC(3) / R  Coordinates of point in local wafer system
C   JWAF   / I  Global wafer index
C
C - Output:
C   VABCXY / I  = VDOK if successful
C               = VDERR if error occurred
C   XYZ(3) / R  Coordinates of point in ALEPH system
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
C
      REAL ABC(3), XYZ(3)
      INTEGER JWAF
C
C     Local variables
      INTEGER IRET
      REAL VUW(3)
C
C     External references:
      INTEGER VABCVU, VVUWXY
C
C ----------------------------------------------------------------------
C
C     First transform to VUW;
C     let VABCVU check the validity of JWAF:
C
      IRET = VABCVU(ABC,JWAF,VUW)
C
C     If that was OK, transform to XYZ:
C
      IF (IRET .EQ. VDOK) THEN
        IRET = VVUWXY(VUW,JWAF,XYZ)
        VABCXY = VDOK
      ELSE
        CALL VZERO(XYZ,3)
        VABCXY = VDERR
      ENDIF
C
      RETURN
      END
#endif
