      REAL FUNCTION EVDEDX(VOLNAM)
C.----------------------------------------------------------------------
CKEY ECALDES VOLUME DEDX / USER
C     H.Videau      creation 08/12/87
C! Gets the DEDX of a volume!
C  This routine looks at the material associated to the volume
C  with name VOLNAM
C   Input :
C           VOLNAM   the name of the volume CH16
C   Output:
C           EVDEDX  the DEDX  in GeV        REAL
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
      CHARACTER*(*) VOLNAM
C Locales.
      INTEGER I,IDEMAT
C
       DO 1 I=1,NEVOL
       IF(EVOLNM(I).EQ.VOLNAM) GO TO 2
 1     CONTINUE
       EVDEDX=0
       GO TO 999
 2     CONTINUE
       IDEMAT=EVOLEM(I)
       EVDEDX=DEDX*EMATAN(IDEMAT)*EMATDE(IDEMAT)/EMATAW(IDEMAT)
C   doit etre ameliore en tenant compte de beta et..
 999  CONTINUE
      END
#endif
