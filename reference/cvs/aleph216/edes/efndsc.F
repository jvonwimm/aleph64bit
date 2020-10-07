      INTEGER FUNCTION EFNDSC(X)
C.----------------------------------------------------------------------
CKEY ECALDES SUBCOMPONENT / USER
C     H.Videau      creation 15/12/85   modification 23/11/88
C! Finds subcomponent number
C  Looks for the sub-component of the calorimeter containing the
C  point X. Returns zero if the point is out of zone.
C   Input :
C           X  point coordinates                         REAL(3)
C   Output:
C           EFNDSC : Sub component #    1<= EFNDSC <= 3  INTEGER
C   Calls: none.
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
      INTEGER I
      REAL X(ECALLS-1)
C
C    Plans limites de SC=SCPLAN
      EFNDSC=1
      DO 1 I=2,ECALSC
        IF(ESCOLI(1,I)*X(1)+ESCOLI(2,I)*X(2)+
     &     ESCOLI(3,I)*X(3)+ESCOLI(4,I)     .LT.0.) EFNDSC=I
 1    CONTINUE
C
      END
#endif
