      INTEGER FUNCTION VIWAFI (JWAF,ILAY,IFAC,IMOD,IWAF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Local layer, face, module, and wafer indices from global wafer
C - Joe Rothberg, December 1993
C
C - Input:
C   JWAF   / I  Global wafer index
C
C - Output:
C   VIWAFI / I  = VDOK if successful
C               = VDERR if error occurred
C   IFAC   / I  Local face index
C   ILAY   / I  Local layer index
C   IMOD   / I  Local module index
C   IWAF   / I  Local wafer index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vgindx.h"
C
      INTEGER JWAF,ILAY,IFAC, IMOD,IWAF
C
C local variables
      INTEGER JMOD, JFAC, JLAY
C
C ----------------------------------------------------------------------
C
      IF ((JWAF.GE.1).AND.(JWAF.LE.NWAFER)) THEN
C
        JMOD = JJMODW(JWAF)
        JFAC = JJFACM(JMOD)
        JLAY = JJLAYF(JFAC)
C
        ILAY = JLAY
        IFAC = JIFACF(JFAC)
        IMOD = JIMODM(JMOD)
        IWAF = JIWAFW(JWAF)
C
        VIWAFI = VDOK
C
      ELSE
C
        VIWAFI = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
