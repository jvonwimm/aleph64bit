      SUBROUTINE EMPACT(VV,NSC,PIN,POU)
C.----------------------------------------------------------------------
CKEY ECALDES NEUTRINO IMPACT / USER
C     H.Videau      Creation 03/04/87   Modification 30/01/88
C! Impacts of a neutrino.
C  This routine computes the NSC*2 intersections of a straight line
C  passing through the origin, having a direction defined by the
C  3-vector V with the envelope of the sensitive part of the
C  encountered modules.
C   Input :
C           V  direction
C   Output:
C           NSC  number of subcomponents touched <3
C           PIN(ECALLS-1,NSC) entrance point
C           POU(ECALLS-1,NSC) exit point
C   Calls: none
C   Called by USER.
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C   Input variables
      REAL VV(3)
C   Output variables
      REAL PIN(3,2),POU(3,2)
      INTEGER NSC
C   External
      INTEGER EFNDMD
      EXTERNAL EFNDMD
C   Locales variables
      INTEGER LSC(2),SC,MD,I,ILAM,J,K
      REAL PV(ECALNP),COSINF(ECALSC),COSSUP(ECALSC)
      REAL W,LAM(2),V(ECALLS-1)
C  Ceci est a changer pronto!!!
      DATA COSINF,COSSUP/.74,-.78,-.983,.983,.78,-.74/
C
C  Normalisons le vecteur d'entree dans V
      W=VV(1)**2+VV(2)**2+VV(3)**2
      IF(W.LE.0) GO TO 999
      W=1./SQRT(W)
      V(1) = VV(1)*W
      V(2) = VV(2)*W
      V(3) = VV(3)*W
      NSC=0
      DO 1 SC= 1,ECALSC
      IF(V(3).LT.COSINF(SC).OR.V(3).GT.COSSUP(SC)) GO TO 1
         MD=EFNDMD(SC,V)
         IF(MD.EQ.0) GO TO 999
      DO 2 I=1,ECALNP
      PV(I)=V(1)*EALILP(1,I,MD,SC)+V(2)*EALILP(2,I,MD,SC)+
     &      V(3)*EALILP(3,I,MD,SC)
 2    CONTINUE
      ILAM=0
C    Boucle sur les points d'intersection
      DO 3 I=1,ECALNP
      IF(ABS(PV(I)).LT.0.00001) GO TO 3
C   Boucle sur les plans pour tester la positivite
      DO 4 J=1,ECALNP
      IF(J.EQ.I) GO TO 4
      W=EALILP(4,J,MD,SC)-EALILP(4,I,MD,SC)*PV(J)/PV(I)
      IF(W.LT.0.) GO TO 3
 4    CONTINUE
      ILAM=ILAM+1
      LAM(ILAM)=-EALILP(4,I,MD,SC)/PV(I)
 3    CONTINUE
      IF(ILAM.EQ.0) GO TO 1
      NSC=NSC+1
      DO 5 K=1,3
      PIN(K,NSC)=AMIN1(LAM(1),LAM(2))*V(K)
      POU(K,NSC)=AMAX1(LAM(1),LAM(2))*V(K)
 5    CONTINUE
 1    CONTINUE
 999    CONTINUE
      END
#endif
