      SUBROUTINE VECT4(R,AA,BB,CC)
C.----------------------------------------------------------------
CKEY ECALDES CROSS PRODUCT / INTERNAL
C.    H.Videau
C! Computes a 4D euclidean X-product
C.----------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
        REAL R(4),AA(4),BB(4),CC(4)
        INTEGER I
        DOUBLE PRECISION A(4),B(4),C(4)
      DO 1 I=1,4
      A(I)=DBLE(AA(I))
      B(I)=DBLE(BB(I))
      C(I)=DBLE(CC(I))
 1    CONTINUE
        R(1)=(A(2)*B(3)-A(3)*B(2))*C(4)+(A(3)*C(2)-A(2)*C(3))*B(4)
     &       +(B(2)*C(3)-B(3)*C(2))*A(4)
        R(2)=(A(3)*B(1)-A(1)*B(3))*C(4)+(A(1)*C(3)-A(3)*C(1))*B(4)
     &       +(B(3)*C(1)-B(1)*C(3))*A(4)
        R(3)=(A(1)*B(2)-A(2)*B(1))*C(4)+(A(2)*C(1)-A(1)*C(2))*B(4)
     &       +(B(1)*C(2)-B(2)*C(1))*A(4)
        R(4)=A(1)*(B(3)*C(2)-B(2)*C(3))+A(2)*(B(1)*C(3)-B(3)*C(1))
     &       +A(3)*(B(2)*C(1)-B(1)*C(2))
        END
#endif
