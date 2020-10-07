      FUNCTION TCHSUM(X,C,N)
C
C-----------------------------------------------------------------------
C! Chebychev sum
C!
C!      Author:    J. Richstein     18-06-84
C!      Modified:  R. Johnson       18-12-86
C!
C!      Called by TBVAL
C!---------------------------------------------------------------------
#ifndef DOC
C
      DIMENSION C(N)
      DATA ZERO,TWO/0.,2./
C
 1000 COF=TWO*X
      A2=ZERO
      A1=ZERO
      A0=C(N)
      IF (N.EQ.1) GO TO 1020
C
        NN=N-1
        DO 1010 K=1,NN
          A2=A1
          A1=A0
          NP1MK=NN+1-K
          A0=C(NP1MK)+COF*A1-A2
 1010   CONTINUE
C
 1020 TCHSUM=(A0-A2)/TWO
C
      RETURN
      END
#endif
