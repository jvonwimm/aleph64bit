      INTEGER FUNCTION VZRSRS (IWAF1,IROS1,IWAF2,IROS2)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX STRIP / USER
C!  Find partner of readout strip in multiplexed z strip channel
C - Steve Wasserbaech, November 1994
C
C - Input:
C   IWAF1  / I  Local wafer index of a z strip
C   IROS1  / I  Readout strip number of a z strip
C
C - Output:
C   VZRSRS / I  = VDOK if successful
C               = VDERR if error occurred or if readout not multiplexed
C   IWAF2  / I  Local wafer index of partner readout strip
C               in strip channel
C   IROS2  / I  Readout strip number of partner readout strip
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vrdoco.h"
#include "vgindx.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER IWAF1, IROS1, IWAF2, IROS2
C
C     Local variables
      INTEGER IROSM1, IROSM2
C
C ----------------------------------------------------------------------
C
C     Make sure the z readout is multiplexed:
C
      IF (.NOT. LZMULT) THEN
        IWAF2 = 0
        IROS2 = 0
        VZRSRS = VDERR
C
C     Check the validity of IWAF1 and IROS1:
C
      ELSEIF ((IWAF1 .LT. 1) .OR. (IWAF1 .GT. NWAFEM) .OR.
     >        (IROS1 .LT. 1) .OR. (IROS1 .GT. NRDSTZ)) THEN
        IWAF2 = 0
        IROS2 = 0
        VZRSRS = VDERR
C
      ELSE
C
C     Compute the readout-strip-in-module index IROSM1, which runs
C     from 1 to 2*NZSROM:
C
        IROSM1 = NRDSTZ*(IWAF1-1) + IROS1
C
C     Count up or down NZSROM readout strips to find the partner:
C
        IF (IROSM1 .LE. NZSROM) THEN
          IROSM2 = IROSM1 + NZSROM
        ELSE
          IROSM2 = IROSM1 - NZSROM
        ENDIF
C
C     Convert IROSM2 to wafer and readout strip:
C
        IWAF2 = (IROSM2-1)/NRDSTZ + 1
        IROS2 = IROSM2 - (IWAF2 - 1)*NRDSTZ
        VZRSRS = VDOK
C
      ENDIF
C
      RETURN
      END
#endif
