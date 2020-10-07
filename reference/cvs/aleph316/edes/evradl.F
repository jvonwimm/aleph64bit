      REAL FUNCTION EVRADL(VOLNAM)
C.----------------------------------------------------------------------
CKEY ECALDES VOLUME RADIATION LENGTH / USER
C     H.Videau      creation 08/12/87
C! Gets the radiation length of a volume!
C  This routine looks at the material associated to the volume
C  with name VOLNAM
C   Input :
C           VOLNAM   the name of the volume CH16
C   Output:
C           EVRADL  the radiation length    REAL
C   Calls: none.
C   Called by USER.
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
C input variable.
      CHARACTER*(*) VOLNAM
C Locales.
      INTEGER I
C
       DO 1 I=1,NEVOL
       IF(EVOLNM(I).EQ.VOLNAM) GO TO 2
 1     CONTINUE
       EVRADL=0
       GO TO 999
 2     CONTINUE
       EVRADL=EMATRL(EVOLEM(I))
 999  CONTINUE
      END
#endif
