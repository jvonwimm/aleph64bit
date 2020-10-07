      INTEGER FUNCTION VIWFFW (IMOD,IWAF,IWFF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates wafer-in-face from local module and local wafer
C - Joe Rothberg, December 1993
C
C - Input:
C   IMOD   / I  Local module index
C   IWAF   / I  Local wafer index
C
C - Output:
C   VIWFFW / I  = VDOK if successful
C               = VDERR if error occurred
C   IWFF   / I  Local wafer-in-face index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER IWFF, IMOD, IWAF
C
C ----------------------------------------------------------------------
C
      IF (IMOD .GE. 1 .AND. IMOD .LE. NMODUL .AND.
     >    IWAF .GE. 1 .AND. IWAF .LE. NWAFEM) THEN
C
        IF (IMOD .EQ. 1) THEN
          IWFF = 1 - IWAF + NWAFEM
        ELSE
          IWFF = IWAF + NWAFEM
        ENDIF
        VIWFFW = VDOK
      ELSE
        VIWFFW =  VDERR
      ENDIF
C
      RETURN
      END
#endif
