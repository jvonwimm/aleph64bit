      FUNCTION ECDETE(A,B,C)
C----------------------------------------------------------------------
C!  - DETERMINANT CALCULATION
C?
C!======================================================================
#ifndef DOC
      DIMENSION A(3),B(3),C(3)
      ECDETE=A(1)*(B(2)*C(3)-B(3)*C(2))
     &- A(2)*(B(1)*C(3)-B(3)*C(1))
     &+ A(3)*(B(1)*C(2)-B(2)*C(1))
      RETURN
      END
#endif
