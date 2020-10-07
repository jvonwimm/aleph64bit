      INTEGER FUNCTION VJWABW (NBWAF, JWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Convert decimal encoded wafer address to global wafer index JWAF
C - Manoj Thulasidas, 5 November 1994
C
C   This function converts a decimal encoded wafer address (as produced
C   by VAENWA) to the corresponding global wafer index JWAF.
C
C - Input:
C   NBWAF  / I  Decimal encoded wafer address
C
C - Output:
C   VJWABW / I  = VDOK if successful
C               = VDERR if error occurred
C   JWAF   / I  Global wafer index
C ----------------------------------------------------------------------
#ifndef DOC
C      IMPLICIT NONE
#include "vglobl.h"
C
C     Arguments:
C
      INTEGER NBWAF, JWAF
C
C     Local variables
C
      INTEGER IRET, ILAY, IWFF, IFAC, IVIEW, IMOD, IWAF
      INTEGER VFWAFF, VJWAFI
C
C ----------------------------------------------------------------------
C
      JWAF = 0
      VJWABW = VDERR
C
C     Decode the bank number:
C
      CALL VADEWA(NBWAF,ILAY,IWFF,IFAC,IVIEW)
C
C     Get the module and wafer numbers from wafer-in-face number:
C
      IRET = VFWAFF(IWFF,IMOD,IWAF)
C
C     Get the global wafer number:
C
      IF (IRET .EQ. VDOK) VJWABW = VJWAFI(ILAY,IFAC,IMOD,IWAF,JWAF)
C
      RETURN
      END
#endif
