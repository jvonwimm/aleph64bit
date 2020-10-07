      INTEGER FUNCTION VRMMOD (MMOD,IWAF,IVIEW)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the readout module index for a given signed global module
C - Joe Rothberg, June 1995
C
C - Input:
C   MMOD   / I  Signed global module index
C   IWAF   / I  Local wafer index
C   IVIEW  / I  View
C
C - Output:
C   VRMMOD / I  Readout module index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C
#include "vglobl.h"
#include "vsltco.h"
#include "vgindx.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER MMOD, IWAF, IVIEW, IROM
C
C     Local variables
      INTEGER IRET, IWFF, IMOD
C
C     External references:
      INTEGER VIWFFW, VIMMOD
C ----------------------------------------------------------------------
C
      IF((MMOD.GE.-NSLOTS).AND.(MMOD.LE.NSLOTS).AND.(MMOD.NE.0))THEN
C
        IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
          IF (LZMULT) THEN
C
C     multiplexing; one readout module per module:
C
            IF (MMOD .LT. 0) IROM = 1
            IF (MMOD .GT. 0) IROM = IROMAX
C
          ELSE
C
C     no multiplexing; one readout module per wafer in face:
C
            IMOD = VIMMOD(MMOD)
            IRET = VIWFFW(IMOD,IWAF,IWFF)
            IROM = IWFF
          ENDIF
C
          VRMMOD = IROM
C
        ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view; one readout module per module:
            IF (MMOD .LT. 0) IROM = 1
            IF (MMOD .GT. 0) IROM = IROMAX
C
          VRMMOD = IROM
C
        ELSE
C
C     invalid view:
          IROM = 0
          VRMMOD = VDERR
C
        ENDIF
C
      ELSE
C
        IROM = 0
        VRMMOD = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
