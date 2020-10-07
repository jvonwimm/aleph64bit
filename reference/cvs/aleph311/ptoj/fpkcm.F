      SUBROUTINE FPKCM(CIN,COUT)
C-----------------------------------------------------------------------
C! Pack an ALEPH track covariance matrix
CKEY PTOJ PACK MATRIX / USER
C!   AUTHOR  :- JUERGEN SPIEKERMANN     8-AUG-1991
C!
C!   INPUTS: CIN
C!        -  COVARIANCE MATRIX IN TRIANGULAR FORM
C!
C!   OUTPUTS: COUT
C!         -  CONVERTED COVARIANCE MATRIX IN TRIANGULAR FORM
C!                      EIGENVALUES ON DIAGONAL
C!                      10 ANGLES DIVIDED BY PI ON OFF DIGONAL PLACES
C!
C!   LIBRARIES REQUIRED: ------
C!
C!   DESCRIPTION
C!   ===========
C!   THIS SUBROUTINE CALCULATES THE EIGENVALUES OF A COVARIANCE MATRIX
C!   AND 10 ROTATION ANGLES WHICH DETERMINE THE MATRIX OF EIGENVECTORS
C!
C!   IT IS USED TO CONVERT THE COVARIANCE MATRIX TO SUCH A FORM, THAT
C!   AFTER UNPACKING WITH SUBROUTINE FUPKCM THE MATRIX IS GUARANTEED
C!   TO BE POSITIVE DEFINITE.
C?
C!======================================================================
#ifndef DOC
      SAVE
#include "alcons.h"
C
C     ----------------- N: DIMENSION OF COVARIANCE-MATRIX
C     ----------------- M:=N*(N+1)/2  DIMENSION OF VECTORS CIN,COUT
      INTEGER N,M,I,J,K,IFAIL
      PARAMETER (N=5,M=N*(N+1)/2)
      REAL CIN(M),COUT(M)

      DOUBLE PRECISION C(N,N),A(N,N),H(N,N),F(N),WORK(N),EW(N),
     &                 DET,CO,SI,A1,A2,PHI
C     ----------------- EMPIRICAL FACTORS (SAME AS IN FUPKCM)
      DATA F/0.266213086238216270D-09,
     &       0.111459714704705784D-04,
     &       0.520405078704231984D-05,
     &       0.170197042982578003D-02,
     &       0.211312784985526782D-01/

C     ----------------- GET MATRIX FROM VECTOR,
C     ----------------- RENORMALIZE WITH EMPIRICAL FACTORS
      DO 10 I=1,N
       DO 20 J=I,N
        C(I,J)=DBLE(CIN(I+J*(J-1)/2))/SQRT(F(I)*F(J))
        C(J,I)=C(I,J)
        A(I,J)=C(I,J)
        A(J,I)=C(J,I)
   20  CONTINUE
   10 CONTINUE
C     ----------------- CALCULATE EIGENVALUES AND EIGENVECTORS
      CALL UTRID(A,N,N,EW,WORK)
      CALL UVTRID(EW,WORK,N,N,A,IFAIL)
      IF (IFAIL.NE.0) GOTO 1000
C     ----------------- EIGENNVALUES TO COUT
      DO 30 I=1,N
       COUT(I*(I+1)/2)=EW(I)
   30 CONTINUE
C     ----------------- RIGHT- OR LEFTHANDED ?
      DO 76 I=1,N
        DO 75 J=1,N
          H(I,J)=A(I,J)
   75   CONTINUE
   76 CONTINUE
#if defined(BIT64)
      CALL RFACT(5,H,5,WORK,JFAIL,DET,IFAIL)
#else
      CALL DFACT(5,H,5,WORK,JFAIL,DET,IFAIL)
#endif
      IF (IFAIL.NE.0) GOTO 1000
C     ----------------- IF A LEFTHANDED THEN MAKE A RIGHTHANDED
      IF (DET.LT.0.D0) THEN
       DO 40 I=1,N
        A(I,1)=-A(I,1)
   40  CONTINUE
      ENDIF
C     ----------------- CALCULATE ROTATION ANGLES
      DO 50 I=1,N-1
       DO 60 J=N,I+1,-1
        IF (A(I,J).EQ.0.D0) THEN
            SI=0.D0
          IF (A(I,J-1).GE.0.D0) THEN
            COUT(I+J*(J-1)/2)=0.
            CO=1.D0
          ELSE
            COUT(I+J*(J-1)/2)=1.
            CO=-1.D0
          ENDIF
        ELSE IF (A(I,J-1).EQ.0.D0) THEN
          CO=0.D0
          SI=SIGN(1.D0,A(I,J))
          COUT(I+J*(J-1)/2)=SIGN(0.5D0,A(I,J))
        ELSE
          PHI=ATAN2(A(I,J),A(I,J-1))
          COUT(I+J*(J-1)/2)=PHI/PI
          CO=COS(PHI)
          SI=SIN(PHI)
        ENDIF
        IF (COUT(I+J*(J-1)/2).GT.1.) THEN
         COUT(I+J*(J-1)/2)=1.
        ENDIF
        IF (COUT(I+J*(J-1)/2).LT.-1.) THEN
         COUT(I+J*(J-1)/2)=-1.
        ENDIF
        DO 70 K=1,N
         A1=CO*A(K,J-1)+SI*A(K,J)
         IF (K.EQ.I) THEN
           A2=0.D0
         ELSE
           A2=-SI*A(K,J-1)+CO*A(K,J)
         ENDIF
         A(K,J-1)=A1
         A(K,J)=A2
   70   CONTINUE
   60  CONTINUE
   50 CONTINUE
      GOTO 2000
 1000 CONTINUE
C     ----------------- FAILURE OF EIGENVALUES AND/OR DETERMINANT
      DO 80 I=1,N
       COUT(I*(I+1)/2)=C(I,I)
       DO 90 J=I+1,N
        COUT(I+J*(J-1)/2)=0.
   90  CONTINUE
   80 CONTINUE
 2000 RETURN
      END
#endif
