      INTEGER FUNCTION VNSCRM (IVIEW)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns number of strip channels in readout module
C - Joe Rothberg, July 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VNSCRM / I  Number of strip channels in readout module
C               = VDERR if IVIEW is invalid
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vdetge.h"
C
      INTEGER IVIEW
C
C ----------------------------------------------------------------------
C
      IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
        VNSCRM = NZSROM
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
        VNSCRM = NPSROM
C
      ELSE
C
C     invalid view:
        VNSCRM = VDERR
      ENDIF
C
      RETURN
      END
#endif
