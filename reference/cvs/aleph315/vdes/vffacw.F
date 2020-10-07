      INTEGER FUNCTION VFFACW (JWAF,JFAC,IWFF)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates global face and wafer-in-face from global wafer
C - Joe Rothberg, December 1993
C
C - Input:
C   JWAF   / I  Global wafer index
C
C - Output:
C   VFFACW / I  = VDOK if successful
C               = VDERR if error occurred
C   JFAC   / I  Global face index
C   IWFF   / I  Local wafer-in-face index
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER IWFF, JFAC, JWAF
C
C  functions
      INTEGER VJFACW
C
C
C ----------------------------------------------------------------------
C
      IF (JWAF .GE. 1 .AND. JWAF .LE. NWAFER) THEN
C
        JFAC = VJFACW(JWAF)
        IWFF = JIWFFW(JWAF)
C
        VFFACW = VDOK
      ELSE
        VFFACW =  VDERR
      ENDIF
C
      RETURN
      END
#endif
