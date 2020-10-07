      SUBROUTINE ELOCAL(ROSE,SC,MD)
C.----------------------------------------------------------------------
CKEY ECALDES LOCAL SYSTEM  / USER
C     H.Videau      Creation 15/12/85   Modification 23/12/85
C                                                 M.Rumpf  03/19/87
C! Defines local system
C  This routine computes the reference planes for the planes,columns and
C  rows of a given module (MD) in a given sub-component (SC) in the
C  system defined by the displacement matrix ROSE.
C   Input :
C           MD    module #          integer
C           SC    sub component #   integer
C           ROSE   displacement matrix  (ECALLS*ECALLS) which can be
C                  filled by EROSYS
C   Output:
C           result stored in comdeck ELOC
C   Calls: EROTPL
C   Called by USER ,ECSGEO (MC),ESHGEO
C.----------------------------------------------------------------------
#ifndef DOC
C
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  Variables d'input
      INTEGER SC,MD
      REAL ROSE(ECALLS,ECALLS)
C   Locales
      INTEGER SS,PN,BARIL
      PARAMETER (BARIL=2)
CC
C     Sauvetage des module et sous-composante courants
      ELOCID=1
      ELOCES=SC
      ELOCEM=MD
C
C  Application du deplacement rose aux plans limitant la zone sensible
        DO 3 PN=1,ECALNP
         CALL EROTPL(ELOCLP(1,PN),EALILP(1,PN,MD,SC),ROSE)
 3      CONTINUE
C     Application du deplacement rose aux divers plans de reference
        IF(SC.NE.BARIL)  CALL EROTPL(ELOCSE(1),EALISE(1,MD,SC),ROSE)
        DO 1 PN=1,ECALLP
         CALL EROTPL(ELOCPL(1,PN),EALIPL(1,PN,MD,SC),ROSE)
         CALL EROTPL(ELOCCL(1,PN),EALICL(1,PN,MD,SC),ROSE)
         IF (SC.EQ.BARIL)        THEN
           SS = 1
           CALL EROTPL(ELOCRW(1,PN,SS),EALIRW(1,PN,SS,MD,SC),ROSE)
         ELSE
           DO 2 SS=1,ECALSS
           CALL EROTPL(ELOCRW(1,PN,SS),EALIRW(1,PN,SS,MD,SC),ROSE)
 2         CONTINUE
         ENDIF
 1      CONTINUE
      END
#endif
