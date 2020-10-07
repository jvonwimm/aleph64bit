      INTEGER FUNCTION VFJFAC (JLAY)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  First global face index in layer
C - Joe Rothberg, December 1993
C
C - Input:
C   JLAY   / I  Global layer index
C
C - Output:
C   VFJFAC / I  First global face index in layer
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER JLAY
C
C ----------------------------------------------------------------------
C
      IF (JLAY .EQ. 1) THEN
        VFJFAC = 1
C
      ELSEIF (JLAY .EQ. 2) THEN
        VFJFAC = NFACEL(1) + 1
      ELSE
        VFJFAC =  VDERR
      ENDIF
C
      RETURN
      END
#endif
