      INTEGER FUNCTION VINTLW (XYZ1,XYZ2,JLAY,IFAC,IWFF,ABCI,ABCO,LINTC)
C ----------------------------------------------------------------------
CKEY VDETDES LINE WAFER / USER
C!  Find intersection points of line and the surface of a wafer
C - Steve Wasserbaech, April 1994
C
C   Given two points, VINTLW considers the line that passes through
C   the points and finds any intersection of the line and the wafer.
C   (Use VINLSW to restrict the search for intersection points to
C   the segment between XYZ1 and XYZ2.)
C
C   Nominal geometry parameters are used in the calculations.
C   No wafer intersection is returned if XYZ1 and XYZ2 are less
C   than 0.0001 cm apart.
C
C - Input:
C   XYZ1(3) / R  (x,y,z) (cm) of first point on line; the line is
C                defined by two points in the Aleph system.
C   XYZ2(3) / R  (x,y,z) (cm) of second point on line
C   JLAY    / I  Global layer index
C   IFAC    / I  Local face index
C   IWFF    / I  Local wafer-in-face index
C
C - Output:
C   VINTLW  / I  = VDOK if successful,
C                  and the line intersects the wafer.
C                = VDERR if an error occurs or the
C                  line does not intersect the wafer.
C   ABCI(3) / R  (a,b,c) coordinates (cm) of wafer entry point
C                in wafer local system
C   ABCO(3) / R  (a,b,c) coordinates (cm) of wafer exit point
C                in wafer local system
C   LINTC   / L  = .TRUE. if the line intersects the wafer on both
C                  of the large surfaces, i.e., the surfaces which are
C                  perpendicular to the c axis.  (If .TRUE., then the
C                  line cannot intersect any other wafers in the face.)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
C
C     Arguments:
      INTEGER JLAY, IFAC, IWFF
      REAL XYZ1(3), XYZ2(3), ABCI(3), ABCO(3)
      LOGICAL LINTC
C
C     Local variables
      INTEGER IRET, I
      REAL ABC1(3), D(3), TMIN, TMAX
C
C     External references:
      INTEGER VINTER
C
C ----------------------------------------------------------------------
C
      VINTLW = VDERR
      CALL VZERO(ABCI,3)
      CALL VZERO(ABCO,3)
      LINTC = .FALSE.
C
C     VINTER checks the validity of the arguments and
C     finds the intersection points:
C
      IRET = VINTER(XYZ1,XYZ2,JLAY,IFAC,IWFF,ABC1,D,TMIN,TMAX,LINTC)
C
      IF (IRET .EQ. VDOK) THEN
C
C     The line does intersect the wafer.
C     Compute the points corresponding to TMIN and TMAX:
C
        DO I=1,3
          ABCI(I) = ABC1(I) + TMIN*D(I)
          ABCO(I) = ABC1(I) + TMAX*D(I)
        ENDDO
        VINTLW = VDOK
C
      ENDIF
C
      RETURN
      END
#endif
