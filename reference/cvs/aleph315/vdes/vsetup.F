      INTEGER FUNCTION VSETUP ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns setup code for last successfully read VDET setup
C - Steve Wasserbaech, November 1994
C
C - Input:
C   (none)
C
C - Output:
C   VSETUP / I  Setup code for last successfully read VDET setup
C               = VDERR if no setup is loaded in commons
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
C ----------------------------------------------------------------------
C
      IF (IVSTUP .GT. 0) THEN
        VSETUP = IVSTUP
C
      ELSE
        VSETUP = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
