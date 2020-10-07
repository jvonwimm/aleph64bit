      LOGICAL FUNCTION VZMULT ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Is the z readout multiplexed?
C - Joe Rothberg, July 1994
C
C - Input:
C   (none)
C
C - Output:
C   VZMULT / L  = .TRUE. if z readout is multiplexed
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vdetge.h"
C
C ----------------------------------------------------------------------
C
      VZMULT = LZMULT
C
      RETURN
      END
#endif
