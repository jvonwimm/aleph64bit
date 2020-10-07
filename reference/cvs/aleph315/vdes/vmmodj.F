      INTEGER FUNCTION VMMODJ (JMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the mglobal module for a given global module
C - Joe Rothberg, January 1994
C
C - Input:
C   JMOD   / I  Global module index
C
C - Output:
C   VMMODJ / I  Signed global module index
C               = 0 if error
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER JMOD
C
C     local variables
      INTEGER MMOD, JFAC, IMOD
C
C ----------------------------------------------------------------------
C
      IF ((JMOD.GE.1).AND.(JMOD.LE.NMODUL)) THEN
C
        JFAC = JJFACM(JMOD)
        IMOD = JIMODM(JMOD)
C
        MMOD = (2*IMOD - 3)*JFAC
C
        VMMODJ = MMOD
C
      ELSE
        VMMODJ = 0
      ENDIF
C
      RETURN
      END
#endif
