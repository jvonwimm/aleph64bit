      SUBROUTINE UMXINV(A,L,M,N,IFAIL)
C----------------------------------------------------------------------
C!  - MODIFIED BY JOE FROM CERNLIB ROUTINE SYMINV TO BE DOUBLE PRECISION
C     Modified: D.Smith    991027  Double precision fixes for Linux
C?
C!======================================================================
#ifndef DOC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(L,M),P(100),Q(100)
C      REAL*8 B(30,30),C(30,30)
      INTEGER R(100)
C
      IFAIL=0
      EPSILN=1.D-14
      IF(L.LT.N) GO TO 95
      IF(M.LT.N) GO TO 96
C
C                   CONSTRUCT TRUTH TABLE
C
      DO 10 I=1,N
   10 R(I)=1
C
C                   BEGIN PROGRAMME
C
      DO 65 I=1,N
      K=0
C
C                   SEARCH FOR PIVOT
C
      BIG=0D0
      DO 37 J=1,N
      TEST=ABS(A(J,J))
      IF(TEST-BIG)37,37,31
   31 CONTINUE
      IBOMB=1
      IF(R(J))100,37,32
   32 BIG=TEST
      K=J
   37 CONTINUE
C
C                   TEST FOR ZERO MATRIX
C
      IBOMB=2
      IF(K)100,100,38
C
C                   TEST FOR LINEARITY
C
   38 IF(I.EQ.1) PIVOT1=A(K,K)
      IBOMB=3
      IF(ABS(A(K,K)/PIVOT1)-EPSILN) 100,39,39
C
C                   PREPARATION FOR ELIMINATION STEP1
C
   39 R(K)=0
      Q(K)=1D0/A(K,K)
      P(K)=1D0
      A(K,K)=0D0
      KP1=K+1
      KM1=K-1
      IBOMB=4
      IF(KM1)100,50,40
   40 DO 49 J=1,KM1
      P(J)=A(J,K)
      Q(J)=A(J,K)*Q(K)
      IF(R(J))100,49,42
   42 Q(J)=-Q(J)
   49 A(J,K)=0D0
   50 IBOMB=5
      IF(K-N)51,60,100
   51 DO 59 J=KP1,N
      P(J)=A(K,J)
      Q(J)=-A(K,J)*Q(K)
      IBOMB=6
      IF(R(J))100,52,59
   52 P(J)=-P(J)
   59 A(K,J)=0D0
C
C                   ELIMINATION PROPER
C
   60 DO 65 J=1,N
      DO 65 K=J,N
   65 A(J,K)=A(J,K)+P(J)*Q(K)
C
C                   ELEMENTS OF LEFT DIAGONAL
C
      DO 70 J=2,N
      JM1=J-1
      DO 70 K=1,JM1
   70 A(J,K)=A(K,J)
C
      RETURN
C
C                   FAILURE RETURN
C
   95 CONTINUE
   96 CONTINUE
  100 IFAIL=1
      RETURN
      END
#endif
