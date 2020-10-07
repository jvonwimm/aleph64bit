      INTEGER FUNCTION VDSCEC (IVIEW,ISCH,IECH)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Calculate electronics channel number from strip channel number
C - Steve Wasserbaech, November 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   ISCH   / I  Strip channel number
C
C - Output:
C   VDSCEC / I  = VDOK if successful
C               = VDERR if error occurred
C   IECH   / I  Electronics channel number
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vrdoco.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER IVIEW, ISCH, IECH
C
C     Local variables
      INTEGER NSROM, IECOR
C
C ----------------------------------------------------------------------
C
      IF ((IVIEW .NE. VVIEWZ) .AND. (IVIEW .NE. VVIEWP)) THEN
C
C     invalid view:
        IECH = 0
        VDSCEC = VDERR
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
C     Check the validity of ISCH:
C
        IF ((ISCH .LT. 1) .OR. (ISCH .GT. NSROM)) THEN
          IECH = 0
          VDSCEC = VDERR
C
        ELSE
C
C     The numbering of electronics channels is flipped if IECOR = -1:
C
          IF (IECOR .EQ. 1) THEN
            IECH = ISCH
          ELSE
            IECH = NSROM + 1 - ISCH
          ENDIF
C
C     Shift IECH down by one so the electronics channel numbers
C     start with zero:
          IECH = IECH - 1
          VDSCEC = VDOK
C
        ENDIF
C
      ENDIF
C
      RETURN
      END
#endif
