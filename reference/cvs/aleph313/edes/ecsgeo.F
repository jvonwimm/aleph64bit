      SUBROUTINE ECSGEO(P,V,SC,MD)
C---------------------------------------------------------------------
CKEY ECALDES SHOWER LOCAL SYSTEM GALEPH / INTERNAL
C     H.Videau      Creation 12/11/86                M.Rumpf 03/19/87
C! Computes shower system
C   Input:
C          P   Coordinates of new frame origin in Aleph system
C          V   Vector along the new X axis in Aleph system
C          SC  Subcomponent # containing P    INTEGER
C          MD  Module #       containing P    INTEGER
C  This routine is used in Galeph environment to compute shower system
C 'GERBE' given shower origin and direction.
C   Calls: EROSYS, ELOCAL
C   Called by USER , EHSHOW in GALEPH
C----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  Variables d'input.
        REAL P(3),V(3)
        INTEGER SC,MD
C   Variable locale
        REAL ROSE(ECALLS,ECALLS)
C
        CALL EROSYS(P,V,ROSE)
        CALL ELOCAL(ROSE,SC,MD)
        END
#endif
