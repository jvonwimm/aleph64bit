      INTEGER FUNCTION VNRFAC (JLAY)
C ----------------------------------------------------------------------
CKEY VDETDES / USER
C!  Number of slots in layer
C - Joe Rothberg, December 1993
C
C - Input:
C   JLAY   / I  Global layer index
C
C - Output:
C   VNRFAC / I  Number of slots in layer
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
      IF (JLAY .EQ. 1 .OR. JLAY .EQ. 2) THEN
C
        VNRFAC = NFACEL(JLAY)
      ELSE
        VNRFAC =  VDERR
      ENDIF
C
      RETURN
      END
#endif
