      REAL FUNCTION VABCFL (JWAF)
C ----------------------------------------------------------------------
CKEY VDETDES FIELD / USER
C!  Return magnetic field in local (a,b,c) coordinates
C - Joe Rothberg, 15 January 1994
C
C - Input:
C   JWAF   / I  Global wafer index
C
C - Output:
C   VABCFL / R  Component of magnetic field in +a direction (kGauss)
C               (returns zero on error)
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER JWAF
C
C  functions
      REAL VMMFLD
C
C     Local variables
C
      INTEGER JMOD,MMOD
      INTEGER VJMODW, VMMODJ
C
C ----------------------------------------------------------------------
C check validity of arguments
C
      IF (JWAF .GE. 1 .AND. JWAF .LE. NWAFER) THEN
C
        JMOD = VJMODW(JWAF)
        MMOD = VMMODJ(JMOD)
C
        VABCFL = VMMFLD(MMOD)
C
      ELSE
C
C     argument JWAF out of range
C
        VABCFL = 0.
C
      ENDIF
C
      RETURN
      END
#endif
