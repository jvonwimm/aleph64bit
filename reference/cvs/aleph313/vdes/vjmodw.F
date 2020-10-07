      INTEGER FUNCTION VJMODW (JWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the global module for a given global wafer
C - Joe Rothberg, December 1993
C
C - Input:
C   JWAF   / I  Global wafer index
C
C - Output:
C   VJMODW / I  Global module index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER JWAF
C
C ----------------------------------------------------------------------
C
      IF ((JWAF.GE.1).AND.(JWAF.LE.NWAFER)) THEN
        VJMODW = JJMODW(JWAF)
      ELSE
        VJMODW = VDERR
      ENDIF
C
      RETURN
      END
#endif
