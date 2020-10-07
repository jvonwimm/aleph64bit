      REAL FUNCTION ELIMST(ST,COTE)
C-----------------------------------------------------------------------
CKEY ECALDES STACK BOUNDARY / USER
C     H.Videau      Creation 02/05/86   Modification 05/02/88
C! Finds stack boundary
C  Gives the distance along the direction of the shower of the origine
C  of the shower to the inferior or superior (as selected by COTE)
C  boundary in depth of the stack designed by ST (unit CM)
C  It uses the fact that the boundaries have been previously defined
C  in the coordinate system of the shower by the routine ECSGEO. The
C  direction of the shower is then along the x axis and if we note t
C  equation of the boundary plane by ax+by+cz+d=0 the distance of th
C  origin to the plane is -d/a
C   Input :
C           ST    Stack # for which boundary is searched (1,2,3) integer
C           COTE    'INF' or 'SUP'
C   Output:
C           ELIMST  Distance along shower axis to the required
C                   boundary of stack ST
C   Calls: none
C   Called by USER
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
C   Variables d'input
         INTEGER ST
         CHARACTER*3 COTE
C
C      Locales
         REAL PLAN(ECALLS)
C
C -------- Execution----------
C
        IF (COTE.EQ.'INF') THEN
        PLAN(1)=-ESSCPC(2,ST,ELOCES)*ELOCPL(1,1)+
     &           ESSCPC(1,ST,ELOCES)*ELOCPL(1,2)
        PLAN(4)=-ESSCPC(2,ST,ELOCES)*ELOCPL(4,1)+
     &           ESSCPC(1,ST,ELOCES)*ELOCPL(4,2)
                           END IF
C
        IF (COTE.EQ.'SUP') THEN
        PLAN(1)=-ESSCPS(2,ST,ELOCES)*ELOCPL(1,1)+
     &           ESSCPS(1,ST,ELOCES)*ELOCPL(1,2)
        PLAN(4)=-ESSCPS(2,ST,ELOCES)*ELOCPL(4,1)+
     &           ESSCPS(1,ST,ELOCES)*ELOCPL(4,2)
                           END IF
C
        ELIMST=-PLAN(4)/PLAN(1)
        END
#endif
