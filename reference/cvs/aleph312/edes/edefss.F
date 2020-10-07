      SUBROUTINE EDEFSS
C.----------------------------------------------------------------------
CKEY ECALDES SECTOR ENDCAP INITIALISATION / INTERNAL
C     H.Videau      Creation 31/12/85   Modification 30/01/86
C                                         M.Rumpf     03/19/87
C! Defines the sector geometry in the module-type,
C  then puts it in place for each slot.
C   Calls: EROTPL
C   Called by EXPNEC
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
C      Locales
      REAL CS,SN
      INTEGER I,J,K,SC,MD,SS
C     Plans temporaires.
      REAL PLTEC(ECALLS),PLTSC(ECALLS),PLTMD(ECALLS)
C
C Passage dans le systeme general des plans de separation pour chaque
C module:   rotations theoriques et alignement.
C   Faire  ECALDM, ROTSC, puis ROTMD, puis EALIAM.
      DO 2060 SC=1,ECALSC,2
      CALL EROTPL(PLTEC,EBPLLF(1,ETSCEB(ESCOET(SC))),ECALDM)
         CALL EROTPL(PLTSC,PLTEC,ETSCDM(1,1,ESCOET(SC)))
         DO 2061 MD=1,ECALMD
            CALL EROTPL(PLTMD,PLTSC,EMODDM(1,1,MD))
            CALL EROTPL(EALISE(1,MD,SC),PLTMD,EALIAM(1,1,MD,SC))
 2061    CONTINUE
 2060 CONTINUE
C
C Calcul des rotations des secteurs.
      CS=COS(ECALAP/2.)
      SN=SIN(ECALAP/2.)
      DO 1 I=1,ECALSS
        DO 2 J=1,ECALLS
          DO 2 K=1,ECALLS
            IF(K.EQ.J)THEN
              ESECDM(K,J,I) = 1.
            ELSE
              ESECDM(K,J,I) = 0.
            ENDIF
    2   CONTINUE
        ESECDM(1,1,I)=CS
        ESECDM(2,2,I)=CS
        ESECDM(1,2,I)= ESECSR(I)*SN
        ESECDM(2,1,I)=-ESECSR(I)*SN
 1    CONTINUE
C
      END
#endif
