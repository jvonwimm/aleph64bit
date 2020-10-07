      INTEGER FUNCTION VDECSC (IVIEW,IECH,ISCH)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Calculate strip channel number from electronics channel number
C - Steve Wasserbaech, November 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   IECH   / I  Electronics channel number
C
C - Output:
C   VDECSC / I  = VDOK if successful
C               = VDERR if error occurred
C   ISCH   / I  Strip channel number
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vrdoco.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER IVIEW, IECH, ISCH
C
C     Local variables
      INTEGER NSROM, IECOR
C
C ----------------------------------------------------------------------
C
      IF ((IVIEW .NE. VVIEWZ) .AND. (IVIEW .NE. VVIEWP)) THEN
C
C     invalid view:
        ISCH = 0
        VDECSC = VDERR
C
      ELSE
C
        IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
          NSROM = NZSROM
          IECOR = IECORZ
C
        ELSE
C
C     r-phi view:
          NSROM = NPSROM
          IECOR = IECORP
C
        ENDIF
C
C     Check the validity of IECH:
C
        IF ((IECH .LT. 0) .OR. (IECH .GT. NSROM-1)) THEN
          ISCH = 0
          VDECSC = VDERR
C
        ELSE
C
C     The numbering of electronics channels is flipped if IECOR = -1;
C     remember that the electronics channel numbers begin with zero:
C
          IF (IECOR .EQ. 1) THEN
            ISCH = IECH
          ELSE
            ISCH = NSROM - 1 - IECH
          ENDIF
C
C     Shift ISCH up by one because the electronics channel numbers
C     start with zero:
          ISCH = ISCH + 1
          VDECSC = VDOK
C
        ENDIF
C
      ENDIF
C
      RETURN
      END
#endif
