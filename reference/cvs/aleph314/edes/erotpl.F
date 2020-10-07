      SUBROUTINE EROTPL(C,A,B)
C.----------------------------------------------------------------------
CKEY ECALDES TENSOR DISPLACEMENT / INTERNAL
C     H.Videau      Creation 15/12/85   Modification le 27/01/88
C
C!  Applies the displacement tensor B to the projective form A
C  memorised in T to give C.
C   Input :
C           A (ECALLS)
C           B (ECALLS,ECALLS)
C   Output:
C           C (ECALLS)
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
      REAL C(ECALLS),A(ECALLS),B(ECALLS,ECALLS),T(ECALLS)
      INTEGER I,J
      DO 2028 I=1,ECALLS
        T(I)=A(I)
 2028 CONTINUE
      DO 2030 I=1,ECALLS
        C(I)=0.
        DO 2029 J=1,ECALLS
                C(I)=C(I)+T(J)*B(J,I)
 2029   CONTINUE
 2030 CONTINUE
      END
#endif
