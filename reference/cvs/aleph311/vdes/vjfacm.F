      INTEGER FUNCTION VJFACM (JMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the global face for a given global module, JMOD
C - Joe Rothberg, December 1993
C
C - Input:
C   JMOD   / I  Global module index
C
C - Output:
C   VJFACM / I  Global face index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER JMOD
C
C ----------------------------------------------------------------------
C
      IF ((JMOD.GE.1).AND.(JMOD.LE.NMODUL)) THEN
        VJFACM = JJFACM(JMOD)
      ELSE
        VJFACM = VDERR
      ENDIF
C
      RETURN
      END
#endif
