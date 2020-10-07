      INTEGER FUNCTION VPHIFC (JFAC,PHI)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns phi of face normal (along +c direction)
C - Steve Wasserbaech, September 1994
C
C   Note: no error condition is returned if the slot JFAC is empty.
C
C - Input:
C   JFAC   / I  Global face index
C
C - Output:
C   VPHIFC / I  = VDOK if successful;
C               = VDERR if JFAC is invalid.
C   PHI    / R  Phi of face normal in the +c direction (radians),
C               calculated from VSLT bank
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "alcons.h"
#include "vglobl.h"
#include "vsltco.h"
#include "vdlaco.h"
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
        VPHIFC = VDERR
        PHI = 0.
C
      ELSE
C
        VPHIFC = VDOK
        JLAY = JJLAYF(JFAC)
        IF (IORIEN(JLAY) .EQ. 1) THEN
          PHI = PHIOFF(JFAC) + PI
          IF (PHI .GT. TWOPI) PHI = PHI - TWOPI
        ELSE
          PHI = PHIOFF(JFAC)
        ENDIF
C
      ENDIF
C
      RETURN
      END
#endif
