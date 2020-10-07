      INTEGER FUNCTION VDYEAR ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Return year (= 91 or 95) of VDET setup
C - Steve Wasserbaech, September 1994
C
C   I don't believe this function should exist!
C
C - Input:
C   (none)
C
C - Output:
C   VDYEAR / I  = 91 if two wafers per module;
C               = 95 if three wafers per module;
C               = VDERR if an error occurred
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
C ----------------------------------------------------------------------
C
      VDYEAR = VDERR
C
      IF (NWAFEM .EQ. 2) THEN
        VDYEAR = 91
      ELSEIF (NWAFEM .EQ. 3) THEN
        VDYEAR = 95
      ENDIF
C
      RETURN
      END
#endif
