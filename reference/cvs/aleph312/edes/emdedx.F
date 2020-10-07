      REAL FUNCTION EMDEDX(MATNAM)
C.----------------------------------------------------------------------
CKEY ECALDES DEDX / USER
C     H.Videau      creation 08/12/87
C! Gets the DEDX of a material!
C  This routine looks at the material with name MATNAM
C   Input :
C           MATNAM   the name of the volume CH16
C   Output:
C           EMDEDX  the DEDX  in GeV        REAL
C   Calls: none.
C   Called by USER.
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
      REAL DEDX
      PARAMETER (DEDX=.000307)
C
C input variable.
      CHARACTER*(*) MATNAM
C Locales.
      INTEGER I
       DO 1 I=1,NEMAT
       IF(EMATMN(I).EQ.MATNAM) GO TO 2
 1     CONTINUE
       EMDEDX=0
       GO TO 999
 2     CONTINUE
       EMDEDX=DEDX*EMATAN(I)*EMATDE(I)/EMATAW(I)
C   doit etre ameliore en tenant compte de beta et..
 999  CONTINUE
      END
#endif
