      INTEGER FUNCTION VNSTCM (IVIEW)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns number of strip channels in module
C - Joe Rothberg, July 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VNSTCM / I  Number of strip channels in module
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
        VNSTCM = NZSMOD
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
        VNSTCM = NPSMOD
C
      ELSE
C
C     invalid view:
        VNSTCM = VDERR
      ENDIF
C
      RETURN
      END
#endif
