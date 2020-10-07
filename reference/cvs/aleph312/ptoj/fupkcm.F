      SUBROUTINE FUPKCM (CIN,COUT)
C-----------------------------------------------------------------------
C!  Unpack covariance matrix of a track
C!
CKEY PTOJ UNPACK/INTERNAL
C!
C!   AUTHOR  :- JUERGEN SPIEKERMANN     8-AUG-1991
C!              THOMAS LOHSE
C!   INPUTS: COUT
C!        -  CONVERTED COVARIANCE MATRIX IN TRIANGULAR FORM
C!                     EIGENVALUES ON DIAGONAL
C!                     10 ANGLES DIVIDED BY PI ON OFF DIGONAL PLACES
C!
C!   OUTPUTS: CIN
C!         -  COVARIANCE MATRIX IN TRIANGULAR FORM
C!
C!
C!   DESCRIPTION
C!   ===========
C!   THIS SUBROUTINE RECONSTRUCTS THE COVARIANCE MATRIX CONVERTED BY
C!   SUBROUTINE FPKCM.
C?
C!======================================================================
#ifndef DOC
C
#include "alcons.h"
C
C     ----------------- N: DIMENSION OF COVARIANCE-MATRIX
C     ----------------- M:=N*(N+1)/2  DIMENSION OF VECTORS CIN,COUT
C
      SAVE
      INTEGER N,M,I,J,K
      PARAMETER (N=5,M=N*(N+1)/2)
C
      REAL CIN(M),COUT(M)
C
      DOUBLE PRECISION A(N,N),F(N),CO,SI,A1,A2,PHI
      DOUBLE PRECISION SQF(N,N)
      LOGICAL FIRST
      DATA    FIRST / .TRUE. /
C     ----------------- EMPIRICAL FACTORS (SAME AS IN FPKCM)
      DATA F/0.266213086238216270D-09,
     &       0.111459714704705784D-04,
     &       0.520405078704231984E-05,
     &       0.170197042982578003E-02,
     &       0.211312784985526782E-01/
C
      IF ( FIRST ) THEN
C
C?   Compute once the DSQRT of the product of the constants
C
        FIRST = .FALSE.
        DO 41 I = 1 , N
          DO 51 J = 1 , N
            SQF(I,J) = DSQRT( F(I)*F(J) )
   51     CONTINUE
   41   CONTINUE
      ENDIF
C     ----------------- CREATE UNITY MATRIX IN A
      DO 30 I=1,N
       A(I,I)=1.D0
       DO 40 J=I+1,N
        A(I,J)=0.D0
        A(J,I)=0.D0
   40  CONTINUE
   30 CONTINUE
C     ----------------- RECONSTRUCT A
      DO 50 I=N-1,1,-1
       DO 60 J=I+1,N
        PHI=CIN(I+J*(J-1)/2)*PI
        CO=DCOS(PHI)
        SI=DSIN(PHI)
        DO 70 K=1,N
         A1=CO*A(K,J-1)-SI*A(K,J)
         A2=SI*A(K,J-1)+CO*A(K,J)
         A(K,J-1)=A1
         A(K,J)=A2
   70   CONTINUE
   60  CONTINUE
   50 CONTINUE
C     ----------------- CALCULATE A*LAMDA*AT
      DO 80 I=1,N
       DO 90 K=I,N
        A1=0.D0
        DO 100 J=1,N
         A1=A1+A(I,J)*A(K,J)*CIN(J*(J+1)/2)
  100   CONTINUE
        COUT(I+K*(K-1)/2)=A1*SQF(I,K)
   90  CONTINUE
   80 CONTINUE
      END
#endif
