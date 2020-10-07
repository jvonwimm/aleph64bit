      INTEGER FUNCTION VNROMM (IVIEW)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns number of readout modules per module in view
C - Joe Rothberg, July 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VNROMM / I  Number of readout modules per module in view
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
        VNROMM = NZROMM
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
        VNROMM = NPROMM
C
      ELSE
C
C     invalid view:
        VNROMM = VDERR
      ENDIF
C
      RETURN
      END
#endif
