      INTEGER FUNCTION VJLAYM (JMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the global layer for a given global module, JMOD
C - Joe Rothberg, December 1993
C
C - Input:
C   JMOD   / I  Global module index
C
C - Output:
C   VJLAYM / I  Global layer index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vgindx.h"
C
      INTEGER JMOD,JFAC
C
C ----------------------------------------------------------------------
C
      IF ((JMOD.GE.1).AND.(JMOD.LE.NMODUL)) THEN
C
        JFAC = JJFACM(JMOD)
        VJLAYM = JJLAYF(JFAC)
C
      ELSE
C
        VJLAYM = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
