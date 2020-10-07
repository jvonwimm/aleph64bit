       INTEGER FUNCTION VIMMOD (MMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the local module index for a given signed global module
C - Joe Rothberg, June 1995
C
C - Input:
C   MMOD   / I  Signed global module index
C
C - Output:
C   VIMMOD / I  Local module index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
C
      INTEGER MMOD
C
C local variables
      INTEGER IMOD
C
C ----------------------------------------------------------------------
C
      IF((MMOD.GE.-NSLOTS).AND.(MMOD.LE.NSLOTS).AND.(MMOD.NE.0)) THEN
C
        IF (MMOD .LT. 0) IMOD = 1
        IF (MMOD .GT. 0) IMOD = 2
C
        VIMMOD = IMOD
C
      ELSE
C
        VIMMOD = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
