      INTEGER FUNCTION VTEXTM (JMOD,TXTM)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the TEXT module for a given global module, JMOD
C - Joe Rothberg, January 1994
C
C - Input:
C   JMOD   / I    Global module index
C
C - Output:
C   VTEXTM / I  = VDOK if successful
C               = VDERR if error occurred
C   TXTM   / C*4  Text module name
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER JMOD
      CHARACTER*4 TXTM
C
C local variables
      INTEGER STATUS, ILAY, IFAC, IMOD
      INTEGER VIMODI
C
C ----------------------------------------------------------------------
C
      IF ((JMOD.GE.1).AND.(JMOD.LE.NMODUL)) THEN
C
        STATUS = VIMODI(JMOD,ILAY,IFAC,IMOD)
        TXTM = TXMODU(ILAY,IFAC,IMOD)
        VTEXTM = VDOK
C
      ELSE
        TXTM = '????'
        VTEXTM = VDERR
      ENDIF
C
      RETURN
      END
#endif
