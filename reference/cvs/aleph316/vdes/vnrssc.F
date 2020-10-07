      INTEGER FUNCTION VNRSSC (IVIEW)
C ----------------------------------------------------------------------
CKEY VDETDES STRIP / USER
C!  Number of readout strips per strip channel
C - Steve Wasserbaech, March 1995
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VNRSSC / I  Number of readout strips per strip channel
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vrdoco.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER IVIEW
C ----------------------------------------------------------------------
C
      IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
C
        VNRSSC = NZRSSC
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
C
        VNRSSC = NPRSSC
C
      ELSE
C
C     invalid view:
C
        VNRSSC = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
