      INTEGER FUNCTION VIMODI (JMOD,ILAY,IFAC,IMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates local layer, face, and module indices from global module
C - Joe Rothberg, December 1993
C
C - Input:
C   JMOD   / I  Global module index
C
C - Output:
C   VIMODI / I  = VDOK if successful
C               = VDERR if error occurred
C   IFAC   / I  Local face index
C   ILAY   / I  Local layer index
C   IMOD   / I  Local module index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vgindx.h"
C
      INTEGER JMOD,ILAY,IFAC, IMOD
C
C local variables
C
C     JFAC   global face index
C
      INTEGER JFAC
C
C ----------------------------------------------------------------------
C
      IF ((JMOD.GE.1).AND.(JMOD.LE.NMODUL)) THEN
C
        IMOD = JIMODM(JMOD)
        JFAC = JJFACM(JMOD)
        ILAY = JJLAYF(JFAC)
        IFAC = JIFACF(JFAC)
C
        VIMODI = VDOK
C
      ELSE
C
        VIMODI = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
