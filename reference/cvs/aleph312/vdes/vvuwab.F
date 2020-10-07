      INTEGER FUNCTION VVUWAB (VUW,JWAF,ABC)
C ----------------------------------------------------------------------
CKEY VDETDES TRANSFORM / USER
C!  Transform wafer coords (v,u,w) into local wafer coords (a,b,c)
C - Joe Rothberg, 15 January 1994
C
C - Input:
C   VUW(3) / R  Coordinates of point in wafer system
C   JWAF   / I  Global wafer index
C
C - Output:
C   VVUWAB / I  = VDOK if successful
C               = VDERR if error occurred
C   ABC(3) / R  Coordinates of point in local wafer system
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vdlaco.h"
#include "vgindx.h"
C
      REAL VUW(3)
      REAL ABC(3)
      INTEGER JWAF
C
C     Local variables
      INTEGER JMOD,MMOD,JLAY
      INTEGER MODSN, IORSN
      INTEGER VJLAYW, VJMODW, VMMODJ
C
C ----------------------------------------------------------------------
C
C check validity of arguments
C
      IF (JWAF .GE. 1 .AND. JWAF .LE. NWAFER) THEN
C
        JLAY = VJLAYW(JWAF)
        JMOD = VJMODW(JWAF)
        MMOD = VMMODJ(JMOD)
C
C sign of z (module)
        MODSN = ISIGN(1,MMOD)
C
C orientation sign, r-phi outward is +1,  r-phi inward is -1
        IORSN = 2*IORIEN(JLAY) - 3
C
C transform
        ABC(1) = MODSN*VUW(3)
        ABC(3) = IORSN*VUW(1)
        ABC(2) = -MODSN*IORSN*VUW(2)
        VVUWAB = VDOK
C
      ELSE
C
C     argument JWAF out of range
C
        CALL VZERO(ABC,3)
        VVUWAB = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
