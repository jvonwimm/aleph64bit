      INTEGER FUNCTION VNRWFF ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Number of wafers in face
C - Steve Wasserbaech, January 1995
C
C - Input:
C   (none)
C
C - Output:
C   VNRWFF / I  Number of wafers per face
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vzpwco.h"
C
C ----------------------------------------------------------------------
      VNRWFF = NWAFEF
C
      RETURN
      END
#endif
