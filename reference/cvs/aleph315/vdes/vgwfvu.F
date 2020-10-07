      SUBROUTINE VGWFVU (NWAF,XYZ,VUW)
C ----------------------------------------------------------------------
CKEY VDETDES ALIGN / USER
C!  Transforms absolute coordinates into wafer coordinates
C - Francesco Forti, 12 October 1990
C - Modified to use new geometry package, S. Wasserbaech, January 1995
C
C - Input:
C   NWAF   / I  Encoded wafer address (a la VAENWA)
C   XYZ(3) / R  Coordinates in ALEPH system (cm)
C
C - Output:
C   VUW(3) / R  Coordinates in wafer system (cm)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgpaal.h"
C
C     Arguments:
C
      INTEGER NWAF
      REAL XYZ(3), VUW(3)
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
      CALL VGTRVE(VTEXPI(1,IFAC,IWFF,ILAY),XYZ,VUW,0.,0.)
C
      RETURN
      END
#endif
