      INTEGER FUNCTION VINTER (XYZ1,XYZ2,JLAY,IFAC,IWFF,
     >                                  ABC1,D,TMIN,TMAX,LINTC)
C ----------------------------------------------------------------------
CKEY VDETDES LINE WAFER / INTERNAL
C!  Find intersection points of line and the surface of a wafer
C - Steve Wasserbaech, November 1994
C
C   VINTER is called by VINTLW and VINLSW to find the intersection
C   points of a line, defined by the points XYZ1 and XYZ2, and a wafer.
C   Points on the line are parametrized as V = ABC1 + T*D, where ABC1
C   is the transformed point XYZ1, and D is the vector in (a,b,c) space
C   from point 1 to point 2.  The intersection points, if any, are
C   expressed in terms of this parametrization.
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
C   VINTER  / I  = VDOK if successful,
C                  and the line intersects the wafer.
C                = VDERR if an error occurs or the
C                  line does not intersect the wafer.
C   ABC1(3) / R  (a,b,c) coordinates (cm) of point XYZ1
C   D(3)    / R  Vector from XYZ1 to XYZ2, expressed in (a,b,c) system
C   TMIN    / R  If the line intersects the wafer, TMIN is the value of
C                T corresponding to the wafer entry point; points are
C                parametrized as V = ABC1 + T*D.
C   TMAX    / R  Value of T corresponding to the exit point
C   LINTC   / L  = .TRUE. if the line intersects the wafer on both
C                  of the large surfaces, i.e., the surfaces which are
C                  perpendicular to the c axis.  (If .TRUE., then the
C                  line cannot intersect any other wafers in the face.)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vzpwco.h"
#include "vwgeco.h"
#include "vgindx.h"
C
C     Arguments:
      INTEGER JLAY, IFAC, IWFF
      REAL XYZ1(3), XYZ2(3), ABC1(3), D(3), TMIN, TMAX
      LOGICAL LINTC
C
C     Parameters:
      REAL EPS, HUGE, AMICR
      PARAMETER (EPS = 1.E-12, HUGE = 1.E20, AMICR = 1.E-4)
C
C     Local variables
      INTEGER JWAF, IRET, I
      REAL ABC2(3), ABCI(3), ABCO(3)
      REAL T1, T2
      REAL SA, SB, SC
      LOGICAL LD0(3)
C
C     External references:
      INTEGER VXYZAB
C
C ----------------------------------------------------------------------
C
      VINTER = VDERR
      CALL VZERO(ABC1,3)
      CALL VZERO(D,3)
      TMIN = 0.
      TMAX = 0.
      LINTC = .FALSE.
C
C     Check that the indices are valid:
C
      IF ((JLAY .LT. 1) .OR. (JLAY .GT. NVLAYR)) GO TO 1000
      IF ((IFAC .LT. 1) .OR. (IFAC .GT. NFACEL(JLAY))) GO TO 1000
      IF ((IWFF .LT. 1) .OR. (IWFF .GT. NWAFEF)) GO TO 1000
C
C     Wafer half-dimensions:
C
      SA = WSIZEA/2.
      SB = WSIZEB/2.
      SC = WTHICK/2.
C
C     Get the global wafer index:
C
      JWAF = IJWFFR(JLAY,IFAC,IWFF)
C
C     Transform the points into the local wafer coordinate system:
C
      IRET = VXYZAB(XYZ1,JWAF,ABC1)
      IF (IRET .EQ. VDERR) GO TO 1000
C
      IRET = VXYZAB(XYZ2,JWAF,ABC2)
      IF (IRET .EQ. VDERR) GO TO 1000
C
C     Find the difference vector:
C
      DO I=1,3
        D(I) = ABC2(I) - ABC1(I)
        LD0(I) = (ABS(D(I)) .LT. EPS)
      ENDDO
C
C     Make sure that the points are different:
C
      IF ((D(1)**2 + D(2)**2 + D(3)**2) .LT. AMICR**2) GO TO 1000
C
C ----------------------------------------------------------------------
C
C     Now start to locate the entrance and exit points.
C     Do this by finding the points at which the line intersects
C     the planes which define the wafer surfaces.  The line is
C     parametrized as V = ABC1 + T*D.
C
C     Find the intersection points of the line and the two large
C     surfaces of the wafer, i.e., those that are perpendicular to the
C     c axis.  T1 and T2 are the values of T at the intersection
C     points:
C
      IF (.NOT. LD0(3)) THEN
C
C     The line does intersect the plane of the wafer somewhere...
C
        T1 = (SC - ABC1(3)) / D(3)
        T2 = (-SC - ABC1(3)) / D(3)
        TMIN = AMIN1(T1,T2)
        TMAX = AMAX1(T1,T2)
C
C     In most cases the line intersects the wafer on these two
C     planes.  We will probably save time if we check this now.
C     Compute the points corresponding to TMIN and TMAX:
C
        DO I=1,3
          ABCI(I) = ABC1(I) + TMIN*D(I)
          ABCO(I) = ABC1(I) + TMAX*D(I)
        ENDDO
        IF ((ABS(ABCI(1)) .LT. SA) .AND. (ABS(ABCI(2)) .LT. SB) .AND.
     >      (ABS(ABCO(1)) .LT. SA) .AND. (ABS(ABCO(2)) .LT. SB)) THEN
          LINTC = .TRUE.
          GO TO 900
        ENDIF
C
      ELSE
C
C     The line is parallel to the plane of the wafer.
C
        IF (ABS(ABC1(3)) .GE. SC) THEN
          GO TO 1000
        ELSE
          TMIN = -HUGE
          TMAX = HUGE
        ENDIF
C
      ENDIF
C
C ----------------------------------------------------------------------
C
C     Now do the same thing for the surfaces perpendicular to
C     the a axis:
C
      IF (.NOT. LD0(1)) THEN
C
C     The line does intersect these surfaces...
C
        T1 = (SA - ABC1(1)) / D(1)
        T2 = (-SA - ABC1(1)) / D(1)
        TMIN = AMAX1(TMIN,AMIN1(T1,T2))
        TMAX = AMIN1(TMAX,AMAX1(T1,T2))
C
      ELSE
C
C     The line is parallel to these surfaces:
C
        IF (ABS(ABC1(1)) .GE. SA) GO TO 1000
C
      ENDIF
C
C     We need TMIN < TMAX to get an intersection:
C
      IF (TMIN .GE. TMAX) GO TO 1000
C
C ----------------------------------------------------------------------
C
C     Now do the same thing for the surfaces perpendicular to
C     the b axis:
C
      IF (.NOT. LD0(2)) THEN
C
C     The line does intersect these surfaces...
C
        T1 = (SB - ABC1(2)) / D(2)
        T2 = (-SB - ABC1(2)) / D(2)
        TMIN = AMAX1(TMIN,AMIN1(T1,T2))
        TMAX = AMIN1(TMAX,AMAX1(T1,T2))
C
      ELSE
C
C     The line is parallel to the plane of the wafer.
C
        IF (ABS(ABC1(2)) .GE. SB) GO TO 1000
C
      ENDIF
C
C     We need TMIN < TMAX to get an intersection:
C
      IF (TMIN .GE. TMAX) GO TO 1000
C
C ----------------------------------------------------------------------
C
C     The line does intersect the wafer.
 900  CONTINUE
      VINTER = VDOK
C
 1000 CONTINUE
      RETURN
      END
#endif
