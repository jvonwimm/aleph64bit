      SUBROUTINE ECPLST(IPLST)
C.---------------------------------------------------------------
CKEY ECALDES PLANE STACK RELATION / USER
C  M.Rumpf                                   September 87
C! Get relation Plane Stack
C   Input : none
C   Output:
C           IPLST(I),I=1,45 contains 10*1,23*2,12*3
C   Stack # = IPLST( plane #)
C   Called by USER,EHSITW in GALEPH at init. time
C.-----------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C   Output variable
      INTEGER IPLST(ECALPL)
C   Locales
      INTEGER I
C
      DO 1 I=1,ECALPL
          IPLST(I) = EPLNES(I)
    1 CONTINUE
       END
#endif
