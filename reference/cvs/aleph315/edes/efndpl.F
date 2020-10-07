      INTEGER FUNCTION EFNDPL(SC,MD,X,SYSTM)
C.----------------------------------------------------------------------
CKEY ECALDES PLANE / USER
C     H.Videau      Creation 15/12/85   Modification 03/02/88
C! Finds plane number
C  Looks in the module MD of the subcomponent SC for the plane
C  containing the point X.
C   Input :
C           SC      sub component #    integer
C           MD      module #                  INTEGER
C           X       point coordinates in      REAL(3)
C           SYSTM  coordinate system name     CHARACTER * 5
C                  ('ALEPH' for example)
C   Output:
C           EFNDPL  Plane #  1<= EFNDPL<= 45  INTEGER
C   Calls: EPLSQL
C   Called by USER    (EHTRKE,EHSHOW in GALEPH)
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C     Input variables
      REAL X(ECALLS-1)
      CHARACTER*5 SYSTM
      INTEGER MD,SC,PL
C    Called routines
      INTEGER EPLSQL
C
C
C     Recherche du plan
      IF(SYSTM.EQ.'ALEPH')   THEN
       PL=EPLSQL(EALIPL(1,1,MD,SC),ECALPL+1,EPSCPC(1,1,SC),X)
                             ELSE
       PL=EPLSQL(ELOCPL(1,1),ECALPL+1,EPSCPC(1,1,SC),X)
                             END IF
      EFNDPL=PL
      END
#endif
