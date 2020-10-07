      INTEGER FUNCTION VDGTOL (XYZ1,XYZ2,JLAY,IFAC,
     >                             NWI,MMOD,IWAF,ABCI,ABCO)
C ----------------------------------------------------------------------
CKEY VDETDES LINE WAFER / USER
C!  Find the intersection points of a line and the wafers in a face
C - Steve Wasserbaech, September 1994
C
C   VDGTOL is a special version of VINTLF.  The calling sequences are
C   identical, but the dimensions of the output arrays are different.
C   VINTLF returns all of the intersections points of a line and the
C   surfaces of the wafers in a face.  VDGTOL returns at most two
C   entry points and two exit points.  (The first two intersected
C   wafers are returned.)
C
C   Furthermore, VDGTOL only returns entry and exit points that lie
C   between XYZ1 and XYZ2.  If, for example, XYZ1 lies outside a wafer
C   and XYZ2 lies inside, ABCI will correspond to the entry point
C   between XYZ1 and XYZ2 (transformed into the ABC system), and ABCO
C   will correspond to XYZ2 itself.  If XYZ1 and XYZ2 both lie within
C   the same wafer, the entry and exit points returned by VDGTOL will
C   correspond to XYZ1 and XYZ2.  These rules are imposed to protect
C   against tracks that enter and exit a wafer through the same
C   surface.  Such tracks would not be correctly handled by the
C   straight-track approximation if we did not limit the entry and
C   exit points in this way.
C
C   VDGTOL calls VINLSW to find the wafer entry and exit points.
C   Nominal geometry parameters are used in the calculations.
C   No wafer intersections are returned if XYZ1 and XYZ2 are less
C   than 0.0001 cm apart.
C
C - Input:
C   XYZ1(3)   / R  (x,y,z) (cm) of first point on line; the line
C                  is defined by two points in the ALEPH system.
C   XYZ2(3)   / R  (x,y,z) (cm) of second point on line
C   JLAY      / I  Global layer index
C   IFAC      / I  Local face index
C
C - Output:
C   VDGTOL    / I  = VDOK if successful and line intersects a wafer;
C                  = VDERR if an error occurs, or line does not
C                    intersect a wafer
C   NWI       / I  Number of wafers intersected
C   MMOD(2)   / I  Signed global module indices of intersected wafers;
C                  1 =< index =< NWI
C   IWAF(2)   / I  Local wafer indices of intersected wafers
C   ABCI(3,2) / R  (a,b,c) coordinates of wafer entry point in IWAF
C                  local system; 1 =< second index =< NWI
C   ABCO(3,2) / R  (a,b,c) coordinates of wafer exit point in IWAF
C                  local system
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vzpwco.h"
#include "vgindx.h"
C
C     Arguments:
      INTEGER JLAY, IFAC, NWI, MMOD(2), IWAF(2)
      REAL XYZ1(3), XYZ2(3), ABCI(3,2), ABCO(3,2)
C
C     Local variables
      INTEGER JWAF, IWFF1, IWFF2, ISTEP, IWFF, I, IRET
      REAL ABCIN(3), ABCOUT(3)
      LOGICAL LINTC
C
C     External references:
      INTEGER VWFFND, VMWAFJ, VINLSW
C
C ----------------------------------------------------------------------
C
      VDGTOL = VDERR
      NWI = 0
      CALL VZERO(MMOD,2)
      CALL VZERO(IWAF,2)
      CALL VZERO(ABCI,3*2)
      CALL VZERO(ABCO,3*2)
C
C     Find the local wafer-in-face index for the wafer nearest to XYZ1:
C
      IWFF1 = VWFFND(XYZ1(3))
C
C     Check whether the line intersects wafer-in-face IWFF1:
C
      IRET = VINLSW(XYZ1,XYZ2,JLAY,IFAC,IWFF1,ABCIN,ABCOUT,LINTC)
      IF (IRET .EQ. VDOK) THEN
C
C     If LINTC = .TRUE. then the line passed through the two large
C     surfaces of the wafer--it cannot intersect any other wafer
C     in this face:
C
        IF (LINTC) THEN
          NWI = NWI + 1
          JWAF = IJWFFR(JLAY,IFAC,IWFF1)
          IRET = VMWAFJ(JWAF,MMOD(NWI),IWAF(NWI))
          DO I=1,3
            ABCI(I,NWI) = ABCIN(I)
            ABCO(I,NWI) = ABCOUT(I)
          ENDDO
          GO TO 900
        ENDIF
      ENDIF
C
C     The line did not intersect the two large surfaces of IWFF1.
C     We will loop over all wafers in the face.  Choose the direction
C     of looping so the order of the intersected wafers corresponds
C     to moving along the line in the direction from XYZ1 toward XYZ2:
C
      IF (XYZ2(3) .GT. XYZ1(3)) THEN
        IWFF1 = 1
        IWFF2 = NWAFEF
      ELSE
        IWFF1 = NWAFEF
        IWFF2 = 1
      ENDIF
C
C     Loop on wafers in the face from IWFF1 to IWFF2 and
C     get the intersection points:
C
      ISTEP = ISIGN(1,IWFF2-IWFF1)
      DO IWFF=IWFF1,IWFF2,ISTEP
        IRET = VINLSW(XYZ1,XYZ2,JLAY,IFAC,IWFF,ABCIN,ABCOUT,LINTC)
        IF ((IRET .EQ. VDOK) .AND. (NWI .LT. 2)) THEN
          NWI = NWI + 1
          JWAF = IJWFFR(JLAY,IFAC,IWFF)
          IRET = VMWAFJ(JWAF,MMOD(NWI),IWAF(NWI))
          DO I=1,3
            ABCI(I,NWI) = ABCIN(I)
            ABCO(I,NWI) = ABCOUT(I)
          ENDDO
        ENDIF
      ENDDO
C
C     Success!
C
 900  CONTINUE
      IF (NWI .GT. 0) VDGTOL = VDOK
C
 1000 CONTINUE
      RETURN
      END
#endif
