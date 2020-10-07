      INTEGER FUNCTION VFWAFI (JWAF,ILAY,IFAC,IWFF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates layer, local face, and wafer-in-face from global wafer
C - Joe Rothberg, December 1993
C
C - Input:
C   JWAF   / I  Global wafer index
C
C - Output:
C   VFWAFI / I  = VDOK if successful
C               = VDERR if error occurred
C   ILAY   / I  Local layer index
C   IFAC   / I  Local face index
C   IWFF   / I  Local wafer-in-face index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER IWFF, IFAC, ILAY, JWAF
      INTEGER STATUS
C
C   local variables
      INTEGER IMOD, IWAF
      INTEGER VIWAFI
C
C ----------------------------------------------------------------------
C
      IF (JWAF .GE. 1 .AND. JWAF .LE. NWAFER) THEN
C
        STATUS = VIWAFI(JWAF,ILAY,IFAC,IMOD,IWAF)
        IWFF = JIWFFW(JWAF)
C
        VFWAFI = VDOK
      ELSE
        VFWAFI =  VDERR
      ENDIF
C
      RETURN
      END
#endif
