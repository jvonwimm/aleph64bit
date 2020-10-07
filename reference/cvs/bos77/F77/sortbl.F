      SUBROUTINE SORTBL(A,IDX,N)
*
*     Sort elements in ascending order (integer elements)
*
*        define indices IDX(1)...IDX(N)
*        CALL SORTBL(A,IDX,N)
*
*     At return the index values are exchanged and corresponds to the
*     ordering
*              A(IDX(1)) =< A(IDX(2)) =< ... =< A(IDX(N))
*     leaving the array A(*) unchanged. If N is given negative, the
*     indices are defined internally as in example below.
*
*     Example: Code to sort an array A(1)...A(N)
*           DO 10 I=1,N
*        10 IDX(I)=I
*           CALL SORTBL(A,IDX,N)
*     or
*           CALL SORTBL(A,IDX,-N)
*
*                                             Author V. Blobel 1989
      INTEGER A(*),IDX(*)
*     ...
      IF(N.LT.0) THEN
*     define sequential pointer for negative N
         DO 10 J=1,-N
   10    IDX(J)=J
      END IF
*     determine initial increment
      M=4
      IF(IABS(N).GT.13) THEN
   20    M=3*M+1
         IF(M.LT.IABS(N)) GOTO 20
         M=(M-1)/3
      END IF
   30 M=(M-1)/3
*     sort with increment M
      DO 50 J=1,IABS(N)-M
      L=J
*     compare elements with indices IDX(L) and IDX(L+M)
   40 IF(A(IDX(L+M)).LT.A(IDX(L))) THEN
*        exchange the two index values
         LL=IDX(L+M)
         IDX(L+M)=IDX(L)
         IDX(L)=LL
         L=L-M
         IF(L.GT.0) GOTO 40
      END IF
   50 CONTINUE
      IF(M.GT.1) GOTO 30
  100 RETURN
      END
