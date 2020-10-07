      INTEGER FUNCTION VABCVU (ABC,JWAF,VUW)
C ----------------------------------------------------------------------
CKEY VDETDES TRANSFORM / USER
C!  Transform local wafer coords (a,b,c) into wafer coords (v,u,w)
C - Joe Rothberg, 15 January 1994
C
C - Input:
C   ABC(3) / R  Coordinates of point in local wafer system
C   JWAF   / I  Global wafer index
C
C - Output:
C   VABCVU / I  = VDOK if successful
C               = VDERR if error occurred
C   VUW(3) / R  Coordinates of point in wafer system
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
C
      INTEGER JMOD,MMOD,JLAY
      INTEGER MODSN, IORSN
      INTEGER VJLAYW, VJMODW, VMMODJ
C
C ----------------------------------------------------------------------
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
        VUW(3) = MODSN*ABC(1)
C
        VUW(1) = IORSN*ABC(3)
C
        VUW(2) = -MODSN*IORSN*ABC(2)
C
        VABCVU = VDOK
C
      ELSE
C
C     argument JWAF out of range
C
        CALL VZERO(VUW,3)
        VABCVU = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
