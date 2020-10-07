      INTEGER FUNCTION VJFACW (JWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the global face for a given global wafer, JWAF
C - Joe Rothberg, December 1993
C
C - Input:
C   JWAF   / I  Global wafer index
C
C - Output:
C   VJFACW / I  Global face index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER JWAF, JMOD
C
C ----------------------------------------------------------------------
C
      IF ((JWAF.GE.1).AND.(JWAF.LE.NWAFER)) THEN
        JMOD = JJMODW(JWAF)
        VJFACW = JJFACM(JMOD)
      ELSE
        VJFACW = VDERR
      ENDIF
C
      RETURN
      END
#endif
