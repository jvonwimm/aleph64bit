      SUBROUTINE EROGEN(ROTMD)
C.----------------------------------------------------------------------
CKEY ECALDES MODULE TYPE SLOTS / INTERNAL
C     H.Videau      Creation 15/12/85   Modification 26/01/88
C! Rotations mod. type -> Slots
C  Generates the theoretical rotations displacing the module type
C  to the slot position for each subcomponent.
C   Output:
C           ROTMD rotation matrix for each slot
C   Called by EDEFMD
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "alcons.h"
#include "ecccom.h"
C   Locales
      INTEGER MD,I,J
      REAL PHI,CS,SN,PHIZER
C   Output
      REAL ROTMD(ECALLS,ECALLS,ECALMD)
C     Calcul des rotations theoriques
      PHIZER=3.5*ECALAP
      DO 2009 MD=1,ECALMD
        DO 1 J=1,ECALLS
          DO 1 I=1,ECALLS
            IF (I.EQ.J) THEN
                  ROTMD(I,J,MD) = 1.
            ELSE
                  ROTMD(I,J,MD) = 0.
            END IF
    1   CONTINUE
        PHI=PHIZER+FLOAT(MD-1)*PIBY6
        CS=COS(PHI)
        SN=SIN(PHI)
        ROTMD(1,1,MD)=CS
        ROTMD(1,2,MD)=SN
        ROTMD(2,1,MD)=-SN
        ROTMD(2,2,MD)=CS
 2009 CONTINUE
      END
#endif
