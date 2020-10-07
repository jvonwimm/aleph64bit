      INTEGER FUNCTION VFAORI (JFAC,IORI)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns orientation of a face
C - Steve Wasserbaech, February 1994
C
C - Input:
C   JFAC    / I  Global face index
C
C - Output:
C   VFAORI  / I  = VDOK if successful;
C                = VDERR if JFAC is invalid.
C   IORI    / I  Face orientation
C                = 1 if the r-phi side faces inward;
C                = 2 if outward.  (Taken from VDLA.)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vdlaco.h"
C
C     Arguments:
      INTEGER JFAC, IORI
C
C ----------------------------------------------------------------------
C
      IF ((JFAC .LT. 1) .OR. (JFAC .GT. NSLOTS)) THEN
C
        VFAORI = VDERR
        IORI = 0
C
      ELSE
C
        VFAORI = VDOK
        IORI = IORIEN(JJLAYF(JFAC))
C
      ENDIF
C
      RETURN
      END
#endif
