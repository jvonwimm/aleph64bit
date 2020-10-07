      INTEGER FUNCTION VJMODM (MMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the global module index for a given mglobal module
C - Joe Rothberg, January 1994
C
C - Input:
C   MMOD   / I  Signed global module index
C
C - Output:
C   VJMODM / I  Global module index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
C
      INTEGER MMOD
C
C local variables
      INTEGER JMOD
C
C ----------------------------------------------------------------------
C
      IF ((MMOD.GE.-NSLOTS).AND.(MMOD.LE.NSLOTS).AND.(MMOD.NE.0)) THEN
C
        JMOD = 2*IABS(MMOD)
        IF (MMOD .LT. 0) JMOD = JMOD - 1
C
        VJMODM = JMOD
C
      ELSE
C
        VJMODM = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
