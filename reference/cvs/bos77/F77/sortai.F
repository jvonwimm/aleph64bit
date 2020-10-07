      SUBROUTINE SORTAI(A,M,N,KEY)
C
C     SUBROUTINE SORTAE
C     -----------------
C     SORTAI SORTS AN ARRAY A(M,N) OF  N  VECTORS,  EACH  OF  LENGTH  M,
C     ACCORDING TO THE ELEMENT A(KEY,I) INTO NONDECREASING ORDER,  USING
C     THE SHELL ALGORITHM. THE KEY ELEMENT IS  ASSUMED  TO  BE  OF  TYPE
C     INTEGER.
C
C
C                         - - - ---
C             CALL SORTAI(A,M,N,KEY)
C                         -
      INTEGER A(M,*),AL1
      MM=N
   30 MM=MM/2
      IF(MM.EQ.0) GOTO 100
      K=N-MM
      J=0
   40 IF(J.GE.K) GOTO 30
      J=J+1
      L2=J
   50 L1=L2+MM
C
C     COMPARE ELEMENTS L1 AND L2
C     --------------------------
      IF(A(KEY,L1).GE.A(KEY,L2)) GOTO 40
C     --------------------------
C
C     EXCHANGE ELEMENTS L1 AND L2
C     ---------------------------
   60 DO 65    I=1,M
      AL1    =A(I,L1)
      A(I,L1)=A(I,L2)
   65 A(I,L2)=AL1
C     ---------------------------
C
      L2=L2-MM
      IF(L2) 40,40,50
C
  100 RETURN
      END
