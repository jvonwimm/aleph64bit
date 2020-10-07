      SUBROUTINE EALTMK
C.----------------------------------------------------------------------
CKEY ECALDES ALIGNMENT MARKS  / INTERNAL
C     H.Videau      creation le 5/12/88
C! Sets the alignment marks in EALIMP
C  Rotates the marks given in EQTY at their position in EALIMP
C   Calls: EROTPT
C   Called by USER (alignement).
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
C   Locales
      INTEGER I,J
      INTEGER MD,SC
C
      DO 3 SC = 1, ECALSC
       DO 4 MD = 1, ECALMD
C     We pick the mire positions in EQTY
        DO 1 I=1,EQTYMN(ESCOEQ(SC))
         CALL EROTPT(EALIMP(1,I,MD,SC),EQTYTP(1,I,ESCOEQ(SC)),
     &             EALITD(1,1,MD,SC))
 1      CONTINUE
 4     CONTINUE
 3    CONTINUE
      END
#endif
