      SUBROUTINE SMAV(R,A,V,N)
C -----------------------------------------------------------
C!    R(N) = A(N,N)*V(N)
C!    AUTHOR: MARTIN POPPE
C -----------------------------------------------------------
#ifndef DOC
      SAVE
C
      DIMENSION R(*),A(*),V(*)
C---> ADDRESSE IN TRIAGULAR MATRIX, ROW II COLUMN KK
      IND(II,KK)=(II*II-II)/2+KK
C---> ADDRESSE IN TRIAGULAR MATRIX, ROW II COLUMN KK
      DO 30 I=1,N
        R(I) = 0.0
        DO 20 K=1,N
          IF(I.GE.K)THEN
            R(I)=R(I)+A(IND(I,K))*V(K)
          ELSE
            R(I)=R(I)+A(IND(K,I))*V(K)
          END IF
  20    CONTINUE
  30  CONTINUE
      RETURN
      END
#endif
