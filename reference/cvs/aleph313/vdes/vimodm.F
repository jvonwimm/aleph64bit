      INTEGER FUNCTION VIMODM (MMOD,ILAY,IFAC)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the layer and local face for a given mglobal module
C - Joe Rothberg, January 1994
C
C - Input:
C   MMOD   / I  Signed global module index
C
C - Output:
C   VIMODM / I  = VDOK if successful
C               = VDERR if error occurred
C   ILAY   / I  Local layer index
C   IFAC   / I  Local face index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
C
      INTEGER MMOD, ILAY, IFAC
C
C local variables
C
      INTEGER JMOD, IMOD, ISTATUS
C functions
      INTEGER VJMODM, VIMODI
C
C ----------------------------------------------------------------------
C
      VIMODM = VDERR
      IF ((MMOD.GE.-NSLOTS).AND.(MMOD.LE.NSLOTS).AND.(MMOD.NE.0)) THEN
C
        JMOD = VJMODM(MMOD)
        ISTATUS = VIMODI(JMOD,ILAY,IFAC,IMOD)
C
        VIMODM = VDOK
      ENDIF
C
      RETURN
      END
#endif
