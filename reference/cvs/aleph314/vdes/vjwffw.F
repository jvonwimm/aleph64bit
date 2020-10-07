      INTEGER FUNCTION VJWFFW (ILAY,IFAC,IWFF,JWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates global wafer from local layer, face, and wafer-in-face
C - Joe Rothberg, December 1993
C
C - Input:
C   ILAY   / I  Local layer index
C   IFAC   / I  Local face index
C   IWFF   / I  Local wafer-in-face index
C
C - Output:
C   VJWFFW / I  = VDOK if successful
C               = VDERR if error occurred
C   JWAF   / I  Global wafer index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vzpwco.h"
#include "vgindx.h"
C
      INTEGER  JWAF, ILAY, IFAC, IWFF
C
C   local variables
      INTEGER JFAC
      INTEGER VJFACI
C
C ----------------------------------------------------------------------
C
      IF (VJFACI(ILAY,IFAC,JFAC).EQ.VDOK .AND.
     >      IWFF .GE. 1 .AND. IWFF .LE. NWAFEF) THEN
C
        JWAF = IJWFFR(ILAY,IFAC,IWFF)
        VJWFFW = VDOK
      ELSE
        VJWFFW =  VDERR
      ENDIF
C
      RETURN
      END
#endif
