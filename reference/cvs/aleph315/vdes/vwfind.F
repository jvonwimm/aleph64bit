      INTEGER FUNCTION VWFIND (XYZ,LOOK,JWAF,ABC,DABC,DIST)
C ----------------------------------------------------------------------
CKEY VDETDES WAFER / USER
C!  Find which wafer in VDET is nearest to a given point
C - Steve Wasserbaech, January 1994
C   Modified 2 June 1995, S. Wasserbaech: ISSFLG = face serial number
C
C   Note: this function correctly handles empty slots.
C
C - Input:
C   XYZ(3)  / R  Coordinates of point in ALEPH system (cm)
C   LOOK    / I  = 0 to select search over all faces;
C                = global face index JFAC to restrict search to one face
C
C - Output:
C   VWFIND  / I  = VDOK if successful
C                = VDERR if an error occurs
C   JWAF    / I  Global wafer index of wafer nearest to point
C   ABC(3)  / R  Coordinates of point in local wafer system JWAF
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
#include "vsltco.h"
#include "vgindx.h"
C
C     Arguments:
      INTEGER LOOK, JWAF
      REAL XYZ(3), ABC(3), DABC(3), DIST
C
C     Parameters:
      REAL HUGE
      PARAMETER (HUGE = 1.E9)
C
C     Local variables
      INTEGER IWFF, JWAF0, IRET, I
      INTEGER ILAY, IFAC, JFACE, JFACE1, JFACE2
      REAL DMIN, DIST0
      REAL ABC0(3), DABC0(3)
C
C     External references:
      INTEGER VWFFND, VWDIST
C
C ----------------------------------------------------------------------
C
      VWFIND = VDERR
      JWAF = 0
      CALL VZERO(ABC,3)
      CALL VZERO(DABC,3)
      DIST = HUGE
C
C     Find the wafer-in-face index IWFF:
C
      IWFF = VWFFND(XYZ(3))
C
C     If LOOK=0, then use the brute force approach: loop over all faces!
C
      IF (LOOK .EQ. 0) THEN
        JFACE1 = 1
        JFACE2 = NSLOTS
      ELSE
        IF ((LOOK .LT. 1) .OR. (LOOK .GT. NSLOTS)) GO TO 1000
        JFACE1 = LOOK
        JFACE2 = LOOK
      ENDIF
C
      DMIN = HUGE
      DO JFACE=JFACE1,JFACE2
        IF (ISSFLG(JFACE) .NE. 0) THEN
          ILAY = JJLAYF(JFACE)
          IFAC = JIFACF(JFACE)
          JWAF0 = IJWFFR(ILAY,IFAC,IWFF)
          IRET = VWDIST(XYZ,JWAF0,ABC0,DABC0,DIST0)
C
          IF (DIST0 .LT. DMIN) THEN
            JWAF = JWAF0
            DMIN = DIST0
            DO I=1,3
              ABC(I) = ABC0(I)
              DABC(I) = DABC0(I)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C
C     If JWAF is still zero, something is wrong!
C
      IF (JWAF .EQ. 0) GO TO 1000
C
C     Success!
C
      DIST = DMIN
      VWFIND = VDOK
C
 1000 CONTINUE
      RETURN
      END
#endif
