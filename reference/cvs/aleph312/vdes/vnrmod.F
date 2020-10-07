      INTEGER FUNCTION VNRMOD ()
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Number of modules in face
C - Joe Rothberg, December 1993
C
C - Input:
C   (none)
C
C - Output:
C   VNRMOD / I  Number of modules per face
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
C
C ----------------------------------------------------------------------
      VNRMOD = NVMODF
C
      RETURN
      END
#endif
