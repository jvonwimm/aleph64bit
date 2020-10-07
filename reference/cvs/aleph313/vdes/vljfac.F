      INTEGER FUNCTION VLJFAC (JLAY)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Last global face index in layer
C - Joe Rothberg, December 1993
C
C - Input:
C   JLAY   / I  Global layer index
C
C - Output:
C   VLJFAC / I  Last global face index in layer
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
        VLJFAC = NFACEL(1)
C
      ELSEIF (JLAY .EQ. 2) THEN
        VLJFAC = NFACEL(1) + NFACEL(2)
      ELSE
        VLJFAC =  VDERR
      ENDIF
C
      RETURN
      END
#endif
