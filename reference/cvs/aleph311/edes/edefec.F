      SUBROUTINE EDEFEC
C.----------------------------------------------------------------------
CKEY ECALDES TILT ROTATION / INTERNAL
C     H.Videau      Creation 15/12/85   Modification 23/01/88
C! Defines ECal (tilt)
C  Creates the displacement matrix ECALDM corresponding to the tilt of
C  the electromagnetic calorimeter respective to Aleph system
C  Called by EXPNEC
C.----------------------------------------------------------------------
#ifndef DOC
C
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
      REAL CS,SN
      INTEGER I,J
C
C   Copie d'abord une matrice unite dans ECALDM,puis introduit la rotati
C
      DO 1 J=1,ECALLS
        DO 1 I=1,ECALLS
          IF(I.EQ.J) THEN
            ECALDM(I,J) = 1.
          ELSE
            ECALDM(I,J) = 0.
          END IF
    1 CONTINUE
      CS=COS(ECALTI)
      SN=SIN(ECALTI)
      ECALDM(1,1)=CS
      ECALDM(1,2)=SN
      ECALDM(2,1)=-SN
      ECALDM(2,2)=CS
      END
#endif
