      FUNCTION DZZZ(ICH)
C-----------------------------------------------------------------
C! Return the d0 of an ENFLW charged object.
C
C  Patrick Janot --  24 June 1991
C-----------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
      dzzz = 0.
      jch = ichefw(ich)
      IF ( jch .GE. kfrtot(1) .AND. jch .LE. klstot(2) )
     .   dzzz = dzero(jch-kfrtot(1)+1)
C
      RETURN
      END
#endif
