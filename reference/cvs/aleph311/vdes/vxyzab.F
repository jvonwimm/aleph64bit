      INTEGER FUNCTION VXYZAB (XYZ,JWAF,ABC)
C ----------------------------------------------------------------------
CKEY VDETDES TRANSFORM / USER
C!  Transform ALEPH coords (x,y,z) into local wafer coords (a,b,c)
C - Steve Wasserbaech, 14 October 1994
C
C - Input:
C   XYZ(3) / R  Coordinates of point in ALEPH system
C   JWAF   / I  Global wafer index
C
C - Output:
C   VXYZAB / I  = VDOK if successful
C               = VDERR if error occurred
C   ABC(3) / R  Coordinates of point in local wafer system
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
C
      REAL XYZ(3), ABC(3)
      INTEGER JWAF
C
C     Local variables
      INTEGER IRET
      REAL VUW(3)
C
C     External references:
      INTEGER VXYZVU, VVUWAB
C
C ----------------------------------------------------------------------
C
C     First transform to VUW;
C     let VXYZVU check the validity of JWAF:
C
      IRET = VXYZVU(XYZ,JWAF,VUW)
C
C     If that was OK, transform to ABC:
C
      IF (IRET .EQ. VDOK) THEN
        IRET = VVUWAB(VUW,JWAF,ABC)
        VXYZAB = VDOK
      ELSE
        CALL VZERO(ABC,3)
        VXYZAB = VDERR
      ENDIF
C
      RETURN
      END
#endif
