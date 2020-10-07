      INTEGER FUNCTION VNELRM (IVIEW)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Returns number of electronics channels in readout module
C - Joe Rothberg, July 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VNELRM / I  Number of electronics channels in readout module
C               = VDERR if IVIEW is invalid
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vrdoco.h"
      INTEGER IVIEW
C
C ----------------------------------------------------------------------
C
      IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
        VNELRM = NZEROM
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
        VNELRM = NPEROM
C
      ELSE
C
C     invalid view:
        VNELRM = VDERR
      ENDIF
C
      RETURN
      END
#endif
