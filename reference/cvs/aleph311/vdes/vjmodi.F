      INTEGER FUNCTION VJMODI (ILAY,IFAC,IMOD,JMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Global module index from the local layer, face, and module indices
C - Joe Rothberg, December 1993
C
C - Input:
C   IFAC   / I  Local face index
C   ILAY   / I  Local layer index
C   IMOD   / I  Local module index
C
C - Output:
C   VJMODI / I  = VDOK if successful
C               = VDERR if error occurred
C   JMOD   / I  Global module index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER JMOD,ILAY,IFAC,IMOD
C
C  functions:
      INTEGER VJFACI
C
C     Local variables
C     JFAC  global face index
C
      INTEGER JFAC
C
C ----------------------------------------------------------------------
C
      IF ((VJFACI(ILAY,IFAC,JFAC).EQ.VDOK).AND.
     >     (IMOD.GE.1).AND.(IMOD.LE.NVMODF)) THEN
C
        JMOD = IJMODU(ILAY,IFAC,IMOD)
        VJMODI = VDOK
      ELSE
        VJMODI = VDERR
      ENDIF
C
      RETURN
      END
#endif
