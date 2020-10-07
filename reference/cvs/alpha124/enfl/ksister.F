      FUNCTION KSISTER(ICH)
C-----------------------------------------------------------------
C! Return the ALPHA index of the sister of a V0 track
C
C  Warning: It works only with V0 tracks!!!
C
C  Patrick Janot - 18 Nov. 1991
C-----------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
      jch = ichefw(ich)
      IF     ( jch .LT. kfrtot(2) ) THEN
        ksister = 0
      ELSEIF ( ich .LE. klstot(2) ) THEN
        ksister = kam(jch-kfrtot(2)+1)
      ELSE
        ksister = 0
      ENDIF
C
      RETURN
      END
#endif
