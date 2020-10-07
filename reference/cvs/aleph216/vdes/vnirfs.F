      INTEGER FUNCTION VNIRFS (IVIEW)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Returns physical strip number of first readout strip
C - Joe Rothberg, July 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VNIRFS / I  Physical strip number of first readout strip
C               = VDERR if IVIEW is invalid
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vrdoco.h"
C
      INTEGER IVIEW
C
C ----------------------------------------------------------------------
C
      IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
        VNIRFS = 1 + NOFRDZ
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
        VNIRFS = 1 + NOFRDP
C
      ELSE
C
C     invalid view:
        VNIRFS = VDERR
      ENDIF
C
      RETURN
      END
#endif
