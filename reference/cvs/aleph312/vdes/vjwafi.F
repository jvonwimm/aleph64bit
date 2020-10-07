      INTEGER FUNCTION VJWAFI (ILAY,IFAC,IMOD,IWAF,JWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Global wafer index from the local layer, face, module and wafer
C - Joe Rothberg, December 1993
C
C - Input:
C   IWAF   / I  Local wafer index
C   ILAY   / I  Local layer index
C   IMOD   / I  Local module index
C   IWAF   / I  Local wafer index
C
C - Output:
C   VJWAFI / I  = VDOK if successful
C               = VDERR if error occurred
C   JWAF   / I  Global wafer index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER JWAF,ILAY,IFAC, IMOD,IWAF
C
C  functions:
      INTEGER VJMODI
C
C     Local variables
C     JMOD  global face index
C
      INTEGER JMOD
C
C ----------------------------------------------------------------------
C
      IF ((VJMODI(ILAY,IFAC,IMOD,JMOD).EQ.VDOK).AND.
     >     (IWAF.GE.1).AND.(IWAF.LE.NWAFEM))  THEN
C
        JWAF = IJWAFR(ILAY,IFAC,IMOD,IWAF)
        VJWAFI = VDOK
      ELSE
        VJWAFI = VDERR
      ENDIF
C
      RETURN
      END
#endif
