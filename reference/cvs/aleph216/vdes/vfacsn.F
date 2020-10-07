      INTEGER FUNCTION VFACSN (JFAC)
C ----------------------------------------------------------------------
CKEY VDETDES FACE / USER
C!  Serial number of face located in slot JFAC
C - Steve Wasserbaech, 2 June 1995
C
C - Input:
C   JFAC   / I  Global face (slot) index
C
C - Output:
C   VFACSN / I  Serial number of face located in slot JFAC
C                = 0 if slot is empty
C                = VDERR if JFAC is invalid
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
C
C     Arguments:
      INTEGER JFAC
C
C ----------------------------------------------------------------------
C
      IF ((JFAC .LT. 1) .OR. (JFAC .GT. NSLOTS)) THEN
C
        VFACSN = VDERR
C
      ELSE
C
        VFACSN = ISSFLG(JFAC)
C
      ENDIF
C
      RETURN
      END
#endif
