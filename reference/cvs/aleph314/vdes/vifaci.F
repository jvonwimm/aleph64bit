      INTEGER FUNCTION VIFACI (JFAC,ILAY,IFAC)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates local layer and face indices from global face
C - Joe Rothberg, December 1993
C
C - Input:
C   JFAC   / I  Global face index
C
C - Output:
C   VIFACI / I  = VDOK if successful
C               = VDERR if error occurred
C   IFAC   / I  Local face index
C   ILAY   / I  Local layer index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vgindx.h"
C
      INTEGER JFAC,ILAY,IFAC
C
C ----------------------------------------------------------------------
C
      IF ((JFAC.GE.1).AND.(JFAC.LE.NSLOTS)) THEN
C
        IFAC = JIFACF(JFAC)
        ILAY = JJLAYF(JFAC)
C
        VIFACI = VDOK
C
      ELSE
C
        VIFACI = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
