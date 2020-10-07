      INTEGER FUNCTION VIROMW (IMOD,IWAF,IVIEW,IROM)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates readout module from module, wafer, and view
C - Steve Wasserbaech, October 1994
C - Input:
C   IMOD   / I  Local module index
C   IWAF   / I  Local wafer index
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VIROMW / I  = VDOK if successful
C               = VDERR if error occurred
C   IROM   / I  Readout module
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER IMOD, IWAF, IVIEW, IROM
C
C     Local variables
      INTEGER IRET, IWFF
C
C     External references:
      INTEGER VIWFFW
C
C ----------------------------------------------------------------------
C
      IF ((IMOD .GE. 1) .AND. (IMOD .LE. NVMODF) .AND.
     >    (IWAF .GE. 1) .AND. (IWAF .LE. NWAFEM)) THEN
C
        IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
          IF (LZMULT) THEN
C
C     multiplexing; one readout module per module:
C
            IF (IMOD .EQ. 1) THEN
              IROM = 1
            ELSE
              IROM = IROMAX
            ENDIF
C
          ELSE
C
C     no multiplexing; one readout module per face:
C
            IRET = VIWFFW(IMOD,IWAF,IWFF)
            IROM = IWFF
          ENDIF
C
          VIROMW = VDOK
C
        ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view; one readout module per module:
          IF (IMOD .EQ. 1) THEN
            IROM = 1
          ELSE
            IROM = IROMAX
          ENDIF
          VIROMW = VDOK
C
        ELSE
C
C     invalid view:
          IROM = 0
          VIROMW = VDERR
C
        ENDIF
C
      ELSE
C
        IROM = 0
        VIROMW = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
