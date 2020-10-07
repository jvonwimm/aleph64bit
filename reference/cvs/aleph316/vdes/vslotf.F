      LOGICAL FUNCTION VSLOTF (JFAC)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Determine whether a given slot contains a face
C - Steve Wasserbaech, February 1994
C   Modified 2 June 1995, S. Wasserbaech: ISSFLG = face serial number
C
C - Input:
C   JFAC   / I  Global index of slot
C
C - Output:
C   VSLOTF / L  = .TRUE. if slot contains a face;
C               = .FALSE. if slot is empty, or if JFAC is invalid
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
        VSLOTF = .FALSE.
C
      ELSE
C
        VSLOTF = (ISSFLG(JFAC) .NE. 0)
C
      ENDIF
C
      RETURN
      END
#endif
