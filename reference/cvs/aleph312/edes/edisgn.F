      SUBROUTINE EDISGN
C.----------------------------------------------------------------------
CKEY ECALDES ROTATION MATRIX / INTERNAL
C     H.Videau      Creation 15/05/87   Modification 26/01/88
C! Rotations matrices
C This routine computes the product of the matrices corresponding
C to the setting of the calorimeter in Aleph : ECALDM
C then the setting of the subcomponents in the calorimeter: ETSCDM,EECBD
C then the setting of the modules in the subcomponents: EMODDM
C then the alignmemt of the modules in their slots: EALIAM
C   Calls: EVFOIS
C   Called by EXPNEC
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  local variables
      REAL VARSC(ECALLS,ECALLS),VARSL(ECALLS,ECALLS)
      INTEGER SC,MD,SY
C
      DO 1 SC=1,ECALSC
       SY=ESCOET(SC)
       IF(SC.EQ.3) THEN
       CALL EVFOIS(ECALDM(1,1),EECBDM(1,1),VARSC(1,1))
       CALL EVFOIS(ETSCDM(1,1,SY),VARSC(1,1),VARSC(1,1))
                    ELSE
       CALL EVFOIS(ETSCDM(1,1,SY),ECALDM(1,1),VARSC(1,1))
                    END IF
       DO 2 MD=1,ECALMD
         CALL EVFOIS(EMODDM(1,1,MD),VARSC(1,1),VARSL(1,1))
         CALL EVFOIS(EALIAM(1,1,MD,SC),VARSL(1,1),EALITD(1,1,MD,SC))
 2    CONTINUE
 1    CONTINUE
      END
#endif
