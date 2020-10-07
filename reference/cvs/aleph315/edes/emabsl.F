      REAL FUNCTION EMABSL(MATNAM)
C.----------------------------------------------------------------------
CKEY ECALDES ABSORPTION LENGTH / USER
C     H.Videau      creation 08/12/87
C! Gets the absorption length of a material!
C  This routine looks at the material with name MATNAM
C   Input :
C           MATNAM   the name of the material CH16
C   Output:
C           EMABSL  the absorption length     REAL
C   Calls: none.
C   Called by USER.
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
C input variable.
      CHARACTER*(*) MATNAM
C Locales.
      INTEGER I
C
       DO 1 I=1,NEMAT
       IF(EMATMN(I).EQ.MATNAM) GO TO 2
 1     CONTINUE
       EMABSL=0
       GO TO 999
 2     CONTINUE
       EMABSL=EMATAL(I)
 999  CONTINUE
      END
#endif
