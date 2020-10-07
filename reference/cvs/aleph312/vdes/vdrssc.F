      INTEGER FUNCTION VDRSSC (IMOD,IWAF,IVIEW,IROS,IROM,ISCH)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Convert readout strip number to strip channel number
C - Steve Wasserbaech, October 1994
C
C - Input:
C   IMOD   / I  Local module index
C   IWAF   / I  Local wafer index
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   IROS   / I  Readout strip number
C
C - Output:
C   VDRSSC / I  = VDOK if successful
C               = VDERR if error occurred
C   IROM   / I  Readout module
C   ISCH   / I  Strip channel number
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vrdoco.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER IMOD, IWAF, IVIEW, IROS, IROM, ISCH
C
C     Local variables:
      INTEGER IRET, IROSIM
C
C     External references:
      INTEGER VIROMW
C
C-----------------------------------------------------------------------
C
C     Calculate IROM; let VIROMW check the validity of IMOD, IWAF,
C     and IVIEW:
C
      IRET = VIROMW(IMOD,IWAF,IVIEW,IROM)
C
      IF (IRET .EQ. VDERR) THEN
        VDRSSC = VDERR
C
      ELSEIF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
C
C     Check the validity of the readout strip number:
C
        IF ((IROS .LT. 1) .OR. (IROS .GT. NRDSTZ)) THEN
          VDRSSC = VDERR
C
        ELSE
          IF (LZMULT) THEN
C
C     Multiplexing in the z readout--one ROM per module.
C     The strip channel number is obtained by converting IWAF,IROS
C     into a "readout-strip-in-module" index IROSIM which runs from
C     1 to 2*NZSROM.  Strip channel I then corresponds to readout-
C     strips-in-module IROSIM = I and I+NZSROM.
C
            IROSIM = NRDSTZ*(IWAF-1) + IROS
            IF (IROSIM .GT. NZSROM) THEN
              ISCH = IROSIM - NZSROM
            ELSE
              ISCH = IROSIM
            ENDIF
            VDRSSC = VDOK
C
          ELSE
C
C     No multiplexing--one ROM per wafer.
C     The strip channel number is equal to the readout strip number:
C
            ISCH = IROS
            VDRSSC = VDOK
          ENDIF
C
        ENDIF
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
C
C     Check the validity of the readout strip number:
C
        IF ((IROS .LT. 1) .OR. (IROS .GT. NRDSTP)) THEN
          VDRSSC = VDERR
C
        ELSE
C
C     The r-phi view is always multiplexed (in a trivial way).
C     The strip channel number is equal to IROS, independent of IWAF.
C
          ISCH = IROS
          VDRSSC = VDOK
C
        ENDIF
C
C-----------------------------------------------------------------------
C
C     Invalid view:
C
      ELSE
        VDRSSC = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
