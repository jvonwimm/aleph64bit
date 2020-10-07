      INTEGER FUNCTION VJLAYW (JWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the global layer for a given global wafer, JWAF
C - Joe Rothberg, December 1993
C
C - Input:
C   JWAF   / I  Global wafer index
C
C - Output:
C   VJLAYW / I  Global layer index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vgindx.h"
C
      INTEGER JWAF,JFAC, JMOD
C
C ----------------------------------------------------------------------
C
      IF ((JWAF.GE.1).AND.(JWAF.LE.NWAFER)) THEN
        JMOD = JJMODW(JWAF)
        JFAC = JJFACM(JMOD)
        VJLAYW = JJLAYF(JFAC)
      ELSE
        VJLAYW = VDERR
      ENDIF
C
      RETURN
      END
#endif
