      INTEGER FUNCTION EFNDRG(SC,MD,X,SYSTM)
C.----------------------------------------------------------------------
CKEY ECALDES REGION / USER
C     H.Videau      Creation 04/01/86   Modification 04/02/88
C! Finds region number
C  Looks for the region of the module MD in the sub-component SC
C  in which the point X is located.
C   Input :
C           SC      sub component #         INTEGER
C           MD      module #                INTEGER
C           X       point coordinates in    REAL(3)
C           SYSTM   coordinate system name  CHARACTER * 5
C                  ('ALEPH' for example)
C   Output:
C           EFNDRG  Region#  1<= EFNDRG<= 4 INTEGER
C   Calls: EPLSCN,EFNDSS.
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  Input variables
      REAL X(ECALLS-1)
      CHARACTER*5 SYSTM
      INTEGER MD,SC
C   Called functions
      INTEGER EFNDSS,EPLSCN
C   Locales
      INTEGER BARIL,SS,RG
      PARAMETER (BARIL=2)
C
C
C
      IF(SC.EQ.BARIL) THEN
                  EFNDRG=ECALRG
                      ELSE
C    Recherche du sous_secteur
         SS=EFNDSS(SC,MD,X,SYSTM)
C    Recherche de la region
         IF(SYSTM.EQ.'ALEPH')     THEN
            RG=EPLSCN(EALIRW(1,1,SS,MD,SC),ECALXG+1,EXRGPC(1,1),X)
                                  ELSE
            RG=EPLSCN(ELOCRW(1,1,SS),ECALXG+1,EXRGPC(1,1),X)
                                  END IF
            IF(RG.EQ.0.OR.RG.GE.ECALXG+1) THEN
                             EFNDRG=0
                               ELSE
                         EFNDRG=EXRGER(RG)
                               END IF
                      END IF
      END
#endif
