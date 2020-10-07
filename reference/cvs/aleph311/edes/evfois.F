      SUBROUTINE EVFOIS(A,B,C)
C.----------------------------------------------------------------------
CKEY ECALDES MATRIX PRODUCT / USER
C     H.Videau      creation 3/07/87    modification 3/07/87
C! Adequate matrix product.
C This routine computes the product of two matrices in the right
C order and permitting one of the input argument to be the output.
C   Input :
C           A,B
C   Output:
C           C
C   Calls: none
C   Called by EDISGN
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
      REAL A(4,4),B(4,4),C(4,4),T(4,4)
      INTEGER I,J,K
      DO 1 I=1,4
      DO 2 J=1,4
      T(I,J)=0
      DO 3 K=1,4
      T(I,J)=T(I,J)+B(I,K)*A(K,J)
 3    CONTINUE
 2    CONTINUE
 1    CONTINUE
      DO 4 I=1,4
      DO 5 J=1,4
      C(I,J)=T(I,J)
 5    CONTINUE
 4    CONTINUE
      END
#endif
