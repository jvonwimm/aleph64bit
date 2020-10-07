      SUBROUTINE VGWFXY (NWAF,VUW,XYZ)
C ----------------------------------------------------------------------
C!  Transforms wafer coordinates into absolute coordinates
CKEY VDETDES ALIGN / USER
C - Francesco Forti, 22 August 1990
C - Modified to use new geometry package, S. Wasserbaech, January 1995
C
C - Input:
C   NWAF   / I  Encoded wafer address (a la VAENWA)
C   VUW(3) / R  Coordinates in wafer system (cm)
C
C - Output:
C   XYZ(3) / R  Coordinates in ALEPH system (cm)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgpaal.h"
C
C     Arguments:
C
      INTEGER NWAF
      REAL VUW(3), XYZ(3)
C
C     Local variables
C
      INTEGER ILAY, IWFF, IFAC, IVIEW
C
C ----------------------------------------------------------------------
C
C     Decode the wafer identifier into the local indices:
C
      CALL VADEWA(NWAF,ILAY,IWFF,IFAC,IVIEW)
C
C     Apply the complete transformation matrix:
C
      CALL VGTRVE(VTEXPD(1,IFAC,IWFF,ILAY),VUW,XYZ,0.,0.)
C
      RETURN
      END
#endif
