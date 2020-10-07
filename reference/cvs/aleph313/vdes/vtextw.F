      INTEGER FUNCTION VTEXTW (JWAF,TXTM)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the TEXT module for a given global wafer, JWAF
C - Joe Rothberg, January 1994
C
C - Input:
C   JWAF   / I    Global wafer index
C
C - Output:
C   VTEXTW / I  = VDOK if successful
C               = VDERR if error occurred
C   TXTM   / C*4  Text module name
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER JWAF
      CHARACTER*4 TXTM
C
C local variables
      INTEGER STATUS, ILAY, IFAC, IMOD, IWAF
      INTEGER VIWAFI
C
C ----------------------------------------------------------------------
C
      IF ((JWAF.GE.1).AND.(JWAF.LE.NWAFER)) THEN
C
        STATUS = VIWAFI(JWAF,ILAY,IFAC,IMOD,IWAF)
        TXTM = TXMODU(ILAY,IFAC,IMOD)
        VTEXTW = VDOK
C
      ELSE
C
        TXTM = '????'
        VTEXTW = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
