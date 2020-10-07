      SUBROUTINE VDWFXY (NWAF,VUW,XYZ)
C ----------------------------------------------------------------------
CKEY VDETDES ALIGN / USER
C!  Converts wafer point into local and absolute coordinates
C - Dave Brown, 21 February 1991
C - Modified to use new geometry package, S. Wasserbaech, January 1995
C
C   This routine flips the local coordinates u and w according to the
C   "readout sign," then calls VGWFXY.  Ugh!
C
C - Input:
C   NWAF   / I  Encoded wafer address (a la VAENWA)
C   VUW(3) / R  Coordinates in wafer system (cm), but not really--
C               the "readout sign" is multiplied onto u and w.
C
C - Output:
C   VUW(3) / R  Coordinates in wafer system (cm), really!
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
      INTEGER ILAY, IWFF, IFAC, IVIEW, JWAF, IRET
      REAL ABC(3)
C
C     External references:
C
      INTEGER VJWFFW, VVUWAB, VNRDIR
C
C ----------------------------------------------------------------------
C
C     Decode the wafer identifier into the local indices:
C
      CALL VADEWA(NWAF,ILAY,IWFF,IFAC,IVIEW)
C
C     Get the global wafer index:
C
      IRET = VJWFFW(ILAY,IFAC,IWFF,JWAF)
C
C     Convert (v,u,w) -> (a,b,c) to get the readout sign information:
C
      IRET = VVUWAB(VUW,JWAF,ABC)
      VUW(2) = ABC(2) * FLOAT(VNRDIR(VVIEWP))
      VUW(3) = ABC(1) * FLOAT(VNRDIR(VVIEWZ))
C
C     Apply the complete transformation matrix:
C
      CALL VGWFXY(NWAF,VUW,XYZ)
C
      RETURN
      END
#endif
