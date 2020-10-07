      INTEGER FUNCTION VPHIFN (JFAC,PHI)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns phi of face normal (toward outside of VDET)
C - Steve Wasserbaech, February 1994
C
C   Note: no error condition is returned if the slot JFAC is empty.
C
C - Input:
C   JFAC   / I  Global face index
C
C - Output:
C   VPHIFN / I  = VDOK if successful;
C               = VDERR if JFAC is invalid.
C   PHI    / R  Phi (radians) of face normal in the +v direction,
C               taken from VSLT bank
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
C
C     Arguments:
      INTEGER JFAC
      REAL PHI
C
C ----------------------------------------------------------------------
C
      IF ((JFAC .LT. 1) .OR. (JFAC .GT. NSLOTS)) THEN
C
        VPHIFN = VDERR
        PHI = 0.
C
      ELSE
C
        VPHIFN = VDOK
        PHI = PHIOFF(JFAC)
C
      ENDIF
C
      RETURN
      END
#endif
