      SUBROUTINE UVTRID(D,E,N,NP,Z,IFAIL)
C----------------------------------------------------------------------
C! Eigenvalues and eigenvectors of real symmetric matrix
C!
CKEY COMPUTE EIGENVALUE /USER
C!
C!   Author   :- J.Speikerman, T.Lohse - 910814  from NUMERICAL RECIPIES
C!
C!   Inputs: D: Diagonal elements of tridiagonal matrix
C!        -  E: N-1 off-diagonal elements of trid. matrix (E(2),...,E(N))
C!              E(1) is abitrary
C!           N: order of tridiagonal matrix
C!           NP: first dimension of array Z
C!           Z: read description
C!
C!   Outputs: D: Eigenvalues of tridiagonal matrix
C!        -   Z: read description
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!   QL algorithm with implicit shifts, to determine the eigenvalues and
C!   eigenvectors of a real, symmetric, tridiagonal matrix, or of a real
C!   symmetric matrix previously reduced by TRID. D is a vector of length
C!   NP. On input it first n elments are the diagonal elements of the
C!   tridiagonal matrix. On output, it returns the eigenvalues. The vector
C!   E inputs the subdiagonal elements of th tridiagonal matrix with E(1)
C!   arbitrary. On output E is destroyed.
C!   If the eigenvectors of the tridiagonal matrix are desired, the matrix
C!   Z is input as the identity matrix.
C!   If the eigenvectors of a previously reduced matrix by TRID are desired
C!   Z is input as the outpt matrix of TRID.
C!   In either case, the kth column of Z returns the normalized eigenvector
C!   corresponding to D(k).
C!   IFAIL is set to zero if the syubroutine is successfully completed.
C!   IFAIL is set to one if more then 30 iterations would be needed.
C?
C!======================================================================
C
#ifndef DOC
      SAVE
      INTEGER I,IFAIL,ITER,K,L,M,N,NP
      DOUBLE PRECISION D(NP),E(NP),Z(NP,NP),B,C,DD,F,G,P,R,S
C
      IFAIL=0
      IF (N.GT.1) THEN
        DO 11 I=2,N
          E(I-1)=E(I)
 11     CONTINUE
        E(N)=0.D0
        DO 15 L=1,N
          ITER=0
  1       DO 12 M=L,N-1
            DD=DABS(D(M))+DABS(D(M+1))
            IF (DABS(E(M))+DD.EQ.DD) GO TO 2
 12       CONTINUE
          M=N
  2       IF (M.NE.L) THEN
            IF (ITER.EQ.30) THEN
              IFAIL=1
              RETURN
            ENDIF
            ITER=ITER+1
            G=(D(L+1)-D(L))/(2.D0*E(L))
            R=DSQRT(G**2+1.D0)
            G=D(M)-D(L)+E(L)/(G+DSIGN(R,G))
            S=1.D0
            C=1.D0
            P=0.D0
            DO 14 I=M-1,L,-1
              F=S*E(I)
              B=C*E(I)
              IF (DABS(F).GE.DABS(G)) THEN
                C=G/F
                R=DSQRT(C**2+1.D0)
                E(I+1)=F*R
                S=1.D0/R
                C=C*S
              ELSE
                S=F/G
                R=DSQRT(S**2+1.D0)
                E(I+1)=G*R
                C=1.D0/R
                S=S*C
              ENDIF
              G=D(I+1)-P
              R=(D(I)-G)*S+2.D0*C*B
              P=S*R
              D(I+1)=G+P
              G=C*R-B
              DO 13 K=1,N
                F=Z(K,I+1)
                Z(K,I+1)=S*Z(K,I)+C*F
                Z(K,I)=C*Z(K,I)-S*F
 13           CONTINUE
 14         CONTINUE
            D(L)=D(L)-P
            E(L)=G
            E(M)=0.D0
            GO TO 1
          ENDIF
 15     CONTINUE
      ENDIF
      RETURN
      END
#endif
