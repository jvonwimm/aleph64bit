      INTEGER FUNCTION VWFFND (Z)
C ----------------------------------------------------------------------
CKEY VDETDES WAFER / USER
C!  Find which wafer in a face is nearest to a given point
C - Steve Wasserbaech, April 1994
C
C   Note: this routine relies on the basic assumptions that the
C   wafer centers have the same set of z coordinates in every face,
C   and that the wafers in a particular face have the same geometry.
C   For a particular point in space (x,y,z) and a particular face JFAC,
C   the local wafer-in-face index IWFF of the wafer nearest to the
C   point is independent of JFAC, x, and y under these assumptions.
C
C - Input:
C   Z      / R  z coordinate (cm) of point in ALEPH system
C
C - Output:
C   VWFFND / I  Local wafer-in-face index of wafer
C               centered nearest to Z
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vzpwco.h"
C
C     Arguments:
      REAL Z
C
C     Local variables
      INTEGER IWFF0
      REAL DZ, DZMIN
C
C ----------------------------------------------------------------------
C
C     Loop over IWFF to find the wafer whose center lies closest
C     to the input value of Z:
C
      VWFFND = 1
      DZMIN = ABS(Z - WAFERZ(1))
      DO IWFF0=2,NWAFEF
        DZ = ABS(Z - WAFERZ(IWFF0))
        IF (DZ .LT. DZMIN) THEN
          VWFFND = IWFF0
          DZMIN = DZ
        ENDIF
      ENDDO
C
      RETURN
      END
#endif
