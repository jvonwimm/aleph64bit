      INTEGER FUNCTION VJLAYF (JFAC)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the global layer index for a given global face, JFAC
C - Joe Rothberg, December 1993
C
C - Input:
C   JFAC   / I  Global face index
C
C - Output:
C   VJLAYF / I  Global layer index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
C
      INTEGER JFAC
C
C ----------------------------------------------------------------------
C
      IF ((JFAC.GE.1).AND.(JFAC.LE.NSLOTS)) THEN
C
        VJLAYF = JJLAYF(JFAC)
C
      ELSE
C
        VJLAYF = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
