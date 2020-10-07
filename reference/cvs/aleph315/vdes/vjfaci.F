      INTEGER FUNCTION VJFACI (ILAY,IFAC,JFAC)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the global face index from the local layer and face
C - Joe Rothberg, December 1993
C
C - Input:
C   IFAC   / I  Local face index
C   ILAY   / I  Local layer index
C
C - Output:
C   VJFACI / I  = VDOK if successful
C               = VDERR if error occurred
C   JFAC   / I  Global face index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER JFAC,ILAY,IFAC
C
C ----------------------------------------------------------------------
C
      IF ((ILAY.GE.1).AND.(ILAY.LE.NVLAYR)
     >     .AND.(IFAC.GE.1).AND.(IFAC.LE.NFACEL(ILAY))) THEN
C
        JFAC = IJFACE(ILAY,IFAC)
C
        VJFACI = VDOK
      ELSE
        VJFACI = VDERR
      ENDIF
C
      RETURN
      END
#endif
