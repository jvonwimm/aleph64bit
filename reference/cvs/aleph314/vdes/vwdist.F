      INTEGER FUNCTION VWDIST (XYZ,JWAF,ABC,DABC,DIST)
C ----------------------------------------------------------------------
CKEY VDETDES WAFER / USER
C!  Find distance from a given point to a given wafer
C - Steve Wasserbaech, January 1994
C
C   Note: this function does not check for empty slots.
C
C - Input:
C   XYZ(3)  / R  Coordinates of point in ALEPH system (cm)
C   JWAF    / I  Global index of wafer
C
C - Output:
C   VWDIST  / I  = VDOK if successful
C                = VDERR if an error occurs
C   ABC(3)  / R  Coordinates of point in local wafer system
C                in JWAF local system
C   DABC(3) / R  DABC(I) = Signed distance from point to limit of wafer
C                along I dimension [I=1,2,3 for a,b,c];
C                DABC(I) = ABS(ABC(I)) - Wafer_Dimension(I)/2;
C                        < 0 if coordinate I lies within limits of
C                            wafer along I dimension;
C                        > 0 if coordinate I lies outside limits of
C                            wafer along I dimension.
C   DIST    / R  Signed distance between the point and the nearest
C                corner, edge, or surface of the wafer;
C                  <0 if inside the wafer,
C                  >0 if outside.
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vwgeco.h"
C
C     Arguments:
      INTEGER JWAF
      REAL XYZ(3), ABC(3), DABC(3), DIST
C
C     Local variables
      INTEGER IRET
      REAL DMIN, DMED, DMAX
C
C     External references:
      INTEGER VXYZAB
C
C ----------------------------------------------------------------------
C
C     Initialize:
C
      VWDIST = VDERR
      CALL VZERO(ABC,3)
      CALL VZERO(DABC,3)
      DIST = 0.
C
C     Convert point coordinates to JWAF local system:
C
      IRET = VXYZAB(XYZ,JWAF,ABC)
C
C     Check for valid wafer index:
C
      IF (IRET .NE. VDOK) GO TO 1000
C
C     Find offsets with respect to edges of wafer:
C
      DABC(1) = ABS(ABC(1)) - WSIZEA/2.
      DABC(2) = ABS(ABC(2)) - WSIZEB/2.
      DABC(3) = ABS(ABC(3)) - WTHICK/2.
C
C     Find smallest, largest, and median components:
C
      DMIN = AMIN1(DABC(1), DABC(2), DABC(3))
      DMAX = AMAX1(DABC(1), DABC(2), DABC(3))
      DMED = DABC(1) + DABC(2) + DABC(3) - DMIN - DMAX
C
C     Compute distance to nearest point on wafer:
C
      IF (DMAX .LE. 0.) THEN
C
C     We are inside the wafer:
        DIST = DMAX
C
      ELSEIF (DMED .LE. 0.) THEN
C
C     We are outside the wafer in one direction only;
C     find the distance to the nearest surface:
        DIST = DMAX
C
      ELSEIF (DMIN .LE. 0.) THEN
C
C     We are outside the wafer in two directions;
C     find the distance to the nearest edge:
        DIST = SQRT(DMED**2 + DMAX**2)
C
      ELSE
C
C     We are outside the wafer in all three directions;
C     find the distance to the nearest corner:
        DIST = SQRT(DMIN**2 + DMED**2 + DMAX**2)
C
      ENDIF
C
C     Success!
C
      VWDIST = VDOK
C
 1000 CONTINUE
      RETURN
      END
#endif
