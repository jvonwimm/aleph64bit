      SUBROUTINE EXPNEC
C.----------------------------------------------------------------------
CKEY ECALDES EXPAND DATA STRUCTURE / USER
C     H.Videau      Creation 26/01/88
C! Expands the data structure at initialisation
C   Called by ECDFRD.
C   Calls: EDEFEC,EDEFSC,EDEFMD,EDEFSS,EALIGN,EDISGN
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C     calorimetre
             CALL EDEFEC
C     sous_composantes (sub-components)
             CALL EDEFSC
C     modules
             CALL EDEFMD
C     generates the alignment
             CALL EALIGN
C     computes the general displacement
             CALL EDISGN
             CALL EALTMK
C     sets the limits of the sensitive zone of the volumes
             CALL ELIMIN
C     sectors
             CALL EDEFSS
C     stacks and planes
             CALL EDEFSP
C     colonnes du baril et des bouchons
             CALL EDEFCL
C     lignes (rows) du baril et des bouchons (end caps)
             CALL EDEFLG
      END
#endif
