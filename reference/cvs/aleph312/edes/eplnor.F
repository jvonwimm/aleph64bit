      SUBROUTINE EPLNOR(PLAN)
C--------------------------------------------------------------------
CKEY ECALDES PLANE NORMALISATION /USER
C     H.Videau      creation 25/10/86
C! Utility routine.
C  Cette routine normalise le plan PLAN de sorte que son vecteur normal
C  soit unitaire.
C   Input :
C           PLAN  (4 coefficients)
C   Output:
C           PLAN  (4 coefficients a,b,c,d with a**2+b**2+c**2=1)
C   Called by USER ,EPLST
C--------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
        REAL PLAN(ECALLS),FNOR
        INTEGER I
        FNOR=PLAN(1)**2+PLAN(2)**2+PLAN(3)**2
        IF(FNOR.LE.0) GO TO 999
        FNOR=1./SQRT(FNOR)
        DO 1 I=1,ECALLS
        PLAN(I)=PLAN(I)*FNOR
 1      CONTINUE
C
  999 RETURN
        END
#endif
