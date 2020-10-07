      INTEGER FUNCTION VJWABR (NBROM, Z, JWAF, NBWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Convert decimal encoded readout module to global wafer index
C - Manoj Thulasidas, 5 November 1994
C - Modified: Steve Wasserbaech, 29 March 1995
C
C   This function converts a decimal encoded readout module address
C   (as produced by VAENWA) to a global wafer index JWAF and the
C   corresponding decimal encoded wafer address.  The layer and
C   face are taken from NBROM.  The wafer-in-face index is calculated
C   from the Z coordinate of the hit.  It is the user's responsibility
C   to ensure that NBROM and Z are consistent.
C
C - Input:
C   NBROM  / I  Decimal encoded readout module address
C   Z      / R  z coordinate of hit (cm)
C
C - Output:
C   VJWABR / I  = VDOK if successful
C               = VDERR if error occurred
C   JWAF   / I  Global wafer index
C   NBWAF  / I  Decimal encoded wafer address
C ----------------------------------------------------------------------
#ifndef DOC
C      IMPLICIT NONE
#include "vglobl.h"
C
C     Arguments:
C
      INTEGER NBROM, JWAF, NBWAF
      REAL Z
C
C     Local variables
C
      INTEGER IRET, ILAY, IROM, IFAC, IVIEW, IWFF
      INTEGER VWFFND, VJWFFW
C
C ----------------------------------------------------------------------
C
      JWAF = 0
      NBWAF = -1
      VJWABR = VDERR
C
C     Decode the readout module address:
C
      CALL VADEWA(NBROM,ILAY,IROM,IFAC,IVIEW)
C
C     Find the wafer-in-face nearest to Z:
C
      IWFF = VWFFND(Z)
C
C     Get the global wafer index:
C
      IRET = VJWFFW(ILAY,IFAC,IWFF,JWAF)
C
C     Encode the wafer address:
C
      IF (IRET .EQ. VDOK) THEN
        CALL VAENWA(NBWAF,ILAY,IWFF,IFAC,IVIEW)
        VJWABR = VDOK
      ENDIF
C
      RETURN
      END
#endif
