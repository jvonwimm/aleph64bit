      SUBROUTINE ETHRU(POINT,V,LEPLAN,PLANES,LENGTH,IMPACT,OUTSID)
C.----------------------------------------------------------------------
CKEY ECALDES IMPACT STRAIGHT LINE VOLUME / USER
C     H.Videau      Creation 09/09/87   Modification
C! Impact on a volume.
C  This routine computes the intersection of a straight line
C  passing through the POINT, having a direction defined by the
C  3-vector V with the convex volume limited by the LEPLAN PLANES.
C  When the point is inside there is always one such intersection.
C   Input :
C           POINT(ECALLS-1)  origine of the vector           ECALLS-1=3
C           V(ECALLS-1)  direction
C           LEPLAN number of planes limiting the volume
C           PLANES(ECALLS,LEPLAN) coefficients of the planes   ECALLS=4
C   LEPLAN and PLANES can be computed with routine EVOLPL which
C   needs only VOLume NAMe as input.
C   Output:
C           LENGTH  distance along the vector between the origin and
C                   the impact
C           IMPACT(ECALLS-1) point of impact
C           OUTSID logical flag
C               OUTSID = .FALSE. when the point is inside the volume
C   Calls: ENORME
C   Called by USER.
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C   Input variables
      INTEGER LEPLAN
      REAL POINT(ECALLS-1),V(ECALLS-1),PLANES(ECALLS,*)
C   Output variables
      REAL LENGTH,IMPACT(ECALLS-1)
      LOGICAL OUTSID
C   Locales variables
      INTEGER LSC(2),I,ILAM,J,K
      REAL PV(10),OP(10)
      REAL W,LAM(2)
C
      CALL ENORME(V)
      DO 2 I=1,LEPLAN
      PV(I)=V(1)*PLANES(1,I)+V(2)*PLANES(2,I)+V(3)*PLANES(3,I)
      OP(I)=POINT(1)*PLANES(1,I)+POINT(2)*PLANES(2,I)+
     &      POINT(3)*PLANES(3,I)+PLANES(4,I)
 2    CONTINUE
      ILAM=0
      OUTSID=.FALSE.
C    Boucle sur les points d'intersection
      DO 3 I=1,LEPLAN
      IF(ABS(PV(I)).LT.0.00001) GO TO 3
C   Boucle sur les plans pour tester la positivite
      DO 4 J=1,LEPLAN
      IF(J.EQ.I) GO TO 4
      W=OP(J)-OP(I)*PV(J)/PV(I)
      IF(W.LT.0.) GO TO 3
 4    CONTINUE
      ILAM=ILAM+1
      LAM(ILAM)=-OP(I)/PV(I)
 3    CONTINUE
      IF(ILAM.EQ.0.OR.LAM(1)*LAM(2).GT.0) THEN
         OUTSID=.TRUE.
      ELSE
         LENGTH=AMAX1(LAM(1),LAM(2))
         DO 5 K=1,ECALLS-1
           IMPACT(K)=LENGTH*V(K)+POINT(K)
 5       CONTINUE
      END IF
      END
#endif
