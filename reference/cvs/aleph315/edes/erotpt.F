      SUBROUTINE EROTPT(C,A,B)
C.----------------------------------------------------------------------
CKEY ECALDES TENSOR DISPLACEMENT / INTERNAL
C     H.Videau      Creation 5/12/88
C
C!  Applies the displacement tensor B to the point A memorised in T to give C.
C   Input :
C           A (ECALLS-1)
C           B (ECALLS,ECALLS)
C   Output:
C           C (ECALLS-1)
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C   Input
      REAL A(ECALLS-1),B(ECALLS,ECALLS)
C  Output
      REAL C(ECALLS-1)
C  Locales
      REAL T(ECALLS),S(ECALLS)
      INTEGER I,J
C
      DO 2028 I=1,ECALLS-1
        T(I)=A(I)
 2028 CONTINUE
        T(4)=1.
      DO 2030 I=1,ECALLS
        S(I)=0.
        DO 2029 J=1,ECALLS
                S(I)=S(I)+T(J)*B(I,J)
 2029   CONTINUE
 2030 CONTINUE
       IF(S(4).NE.0.) THEN
         DO 2031 I=1,ECALLS-1
          C(I)=S(I)/S(4)
 2031    CONTINUE
                      END IF
      END
#endif
