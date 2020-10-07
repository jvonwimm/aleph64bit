           SUBROUTINE HCHECK(X,IDEAD)
C---------------------------------------------------
CKEY HCALDES HCAL  STATUS POINT / USER
C
C!   Check if a point is inside Hcal
C!
C!                        Authors: C.Capon,G.Catanesi
C!
C!   Input : X/R Coordinates in the Aleph R.S.
C!
C!   output: IDEAD/I = if 0 the point is inside
C!
C!--------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "hcgeom.h"
           DIMENSION X(*)
C
        PHI=ATAN2(X(2),X(1))
        IF (PHI.LT.0.) PHI=PHI+TWOPI
C?              go to local coordinates : yl is // to barrel module depth
        YL=X(1)*COS(PHI)+X(2)*SIN(PHI)
        XL=X(1)*SIN(PHI)-X(2)*COS(PHI)
        Z=X(3)
        ZABS=ABS(Z)
C
C?              test if point is inside Hcal
C
           IDEAD=0
           IF(ZABS.GT.ZENDMX)THEN
                   IDEAD=1
           ELSEIF(YL.GT.YBARMX)THEN
                   IDEAD=2
           ELSEIF(ZABS.GT.HCZMAX(1).AND.YL.GT.HCRMAX(3))THEN
                   IDEAD=3
           ELSEIF(YL.LT.HCRMIN(2))THEN
                   IDEAD=4
           ELSEIF(ZABS.LT.HCZMIN(2).AND.YL.LT.HCRMAX(2))THEN
                   IDEAD=5
          ELSEIF(HCRMAX(2).LT.YL.AND.YL.LT.HCRMIN(1)
     *    .AND.ZABS.LT.HCZMIN(3))THEN
                   IDEAD=6
           ELSEIF(HCZMAX(1).LT.ZABS.AND.ZABS.LT.HCZMIN(3))THEN
                   IDEAD=7
           ENDIF
C
           RETURN
           END
#endif
