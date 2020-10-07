      FUNCTION EOWFAR(POINT,LEPLAN,PLANES)
C----------------------------------------------------------------------
CKEY ECALDES DISTANCE BORDER / USER
C     H.Videau      Creation 09/09/87   Modification  09/09/87
C! Distance to the nearest border
C  Computes the shortest distance from the point POINT to the set of
C  LEPLAN planes PLANES limiting the convex volume of interest.
C  If LEPLAN is negative the point is outside.
C   Input :
C           POINT : point 3 coordinates
C           LEPLAN: number of planes limiting the volume
C           PLANES(4,LEPLAN): planes coefficients
C   Output:
C           EOWFAR : distance
C  LEPLAN and PLANES can be computed with routine EVOLPL which needs
C  only VOLume NAMe as input.
C   Calls: none
C   Called by USER.
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
C    input variables
      INTEGER LEPLAN
      REAL POINT(ECALLS-1), PLANES(ECALLS,*)
C    output
      REAL EOWFAR
C    locales
      INTEGER I
      REAL DIST,MIND
C
      MIND = 9999.
      DO 1 I=1,LEPLAN
      DIST= POINT(1)*PLANES(1,I)+POINT(2)*PLANES(2,I)+
     &      POINT(3)*PLANES(3,I)+PLANES(4,I)
      IF(MIND.GT.DIST)MIND=DIST
 1    CONTINUE
      EOWFAR=MIND
      END
#endif
