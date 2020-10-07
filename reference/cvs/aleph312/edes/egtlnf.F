      SUBROUTINE EGTLNF(PLANNM,PLAN)
C.----------------------------------------------------------------------
CKEY ECALDES PLANE ELNF / INTERNAL
C     H.Videau      Creation 02/02/88
C! Gets a plane from ELNF by its name
C   Input :
C           PLANNM    name of the plane
C   Output:
C           PLAN      the 4 coefficients of the plane
C   Calls: none
C   Called by USER ,EGPTLG
C.----------------------------------------------------------------------
#ifndef DOC
C
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Input
      CHARACTER*(*) PLANNM
C    Output
      REAL PLAN(ECALLS)
C    Locales
      INTEGER IDELNF,I
C
      DO 1 IDELNF=1,NELNF
        IF(ELNFNM(IDELNF).EQ.PLANNM) THEN
          DO 3 I=1,ECALLS
            PLAN(I)=ELNFLF(I,IDELNF)
 3        CONTINUE
          GO TO 999
                                      END IF
 1    CONTINUE
999   CONTINUE
      END
#endif
