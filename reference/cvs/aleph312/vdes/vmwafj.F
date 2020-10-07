      INTEGER FUNCTION VMWAFJ (JWAF,MMOD,IWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Mglobal module index and local wafer from the global wafer index
C - Joe Rothberg, January 1994
C
C - Input:
C   JWAF   / I  Global wafer index
C
C - Output:
C   VMWAFJ / I  = VDOK if successful
C               = VDERR if error occurred
C   MMOD   / I  Signed global module index
C   IWAF   / I  Local wafer index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER JWAF, MMOD, IWAF
C
C     Local variables
      INTEGER JMOD
C
C functions
      INTEGER VMMODJ
C
C ----------------------------------------------------------------------
C
      IF ((JWAF.GE.1).AND.(JWAF.LE.NWAFER)) THEN
C
        JMOD = JJMODW(JWAF)
        MMOD = VMMODJ(JMOD)
        IWAF = JIWAFW(JWAF)
C
        VMWAFJ = VDOK
C
      ELSE
C
        VMWAFJ = VDERR
        MMOD = 0
        IWAF = 0
C
      ENDIF
C
      RETURN
      END
#endif
