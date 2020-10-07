      INTEGER FUNCTION VPHICF (JFAC,PHI)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns phi coordinate of face center
C - Steve Wasserbaech, 7 August 1995
C
C   Note: no error condition is returned if the slot JFAC is empty.
C
C - Input:
C   JFAC   / I  Global face index
C
C - Output:
C   VPHICF / I  = VDOK if successful;
C               = VDERR if JFAC is invalid.
C   PHI    / R  Phi coordinate of face center (radians)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER JFAC
      REAL PHI
C
C     Local variables
      INTEGER JLAY
C
C ----------------------------------------------------------------------
C
      IF ((JFAC .LT. 1) .OR. (JFAC .GT. NSLOTS)) THEN
C
        VPHICF = VDERR
        PHI = 0.
C
      ELSE
C
        VPHICF = VDOK
        PHI = WAPHIC(JFAC)
C
      ENDIF
C
      RETURN
      END
#endif
