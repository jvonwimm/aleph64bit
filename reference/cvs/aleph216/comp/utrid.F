      SUBROUTINE UTRID(A,N,NP,D,E)
C----------------------------------------------------------------------
C! Householder reduction of a real, symmetric matrix
C!
CKEY COMPUTE EIGENVALUE / USER
C!
C!   Author   : J.Spiekerman, T.Lohse - 910814    from NUMERICAL RECIPIES
C!
C!   Inputs:- A:  matrix which is transformed
C!            N:  order of matrix a
C!           NP:  first dimension of array a
C!
C!   Outputs: D: vector of the n diagonal-elements of tridiagonal matrix
C!        -   E: vector of the n-1 off diagonal-elements of trid. matrix
C!            A: matrix which performs the transformation
C!
C!   Description
C!   ===========
C!   Householder reduction of a real, symmetric, N by N matrix a, stored
C!   in an NP by at least N physical array. On output a is replaced by
C!   the orthogonal matrix Q effecting the transformation. D returns the
C!   diagonal elements of the tridiagonal matrix, and E the N-1 off-diagonal
C!   elements (E(2),...,E(N)). E(1) is set to zero.
C?
C!======================================================================
C
#ifndef DOC
      SAVE
      INTEGER I,J,K,L,M,N,NP
      DOUBLE PRECISION A(NP,NP),D(NP),E(NP),F,G,H,HH,SCALE
C
      IF (N.GT.1) THEN
        DO 18 I=N,2,-1
          L=I-1
          H=0.D0
          SCALE=0.D0
          IF (L.GT.1) THEN
            DO 11 K=1,L
              SCALE=SCALE+DABS(A(I,K))
   11       CONTINUE
            IF (SCALE.EQ.0.D0) THEN
              E(I)=A(I,L)
            ELSE
              DO 12 K=1,L
                A(I,K)=A(I,K)/SCALE
                H=H+A(I,K)**2
   12         CONTINUE
              F=A(I,L)
              G=-DSIGN(DSQRT(H),F)
              E(I)=SCALE*G
              H=H-F*G
              A(I,L)=F-G
              F=0.D0
              DO 15 J=1,L
                A(J,I)=A(I,J)/H
                G=0.D0
                DO 13 K=1,J
                  G=G+A(J,K)*A(I,K)
   13           CONTINUE
                IF (L.GT.J) THEN
                  DO 14 K=J+1,L
                    G=G+A(K,J)*A(I,K)
   14             CONTINUE
                ENDIF
                E(J)=G/H
                F=F+E(J)*A(I,J)
   15         CONTINUE
              HH=F/(H+H)
              DO 17 J=1,L
                F=A(I,J)
                G=E(J)-HH*F
                E(J)=G
                DO 16 K=1,J
                  A(J,K)=A(J,K)-F*E(K)-G*A(I,K)
   16           CONTINUE
   17         CONTINUE
            ENDIF
          ELSE
            E(I)=A(I,L)
          ENDIF
          D(I)=H
   18   CONTINUE
      ENDIF
      D(1)=0.D0
      E(1)=0.D0
      DO 23 I=1,N
        L=I-1
        IF (D(I).NE.0.D0) THEN
          DO 21 J=1,L
            G=0.D0
            DO 19 K=1,L
              G=G+A(I,K)*A(K,J)
   19       CONTINUE
            DO 20 K=1,L
              A(K,J)=A(K,J)-G*A(K,I)
   20       CONTINUE
   21     CONTINUE
        ENDIF
        D(I)=A(I,I)
        A(I,I)=1.D0
        IF (L.GE.1) THEN
          DO 22 J=1,L
            A(I,J)=0.D0
            A(J,I)=0.D0
   22     CONTINUE
        ENDIF
   23 CONTINUE
      RETURN
      END
#endif
