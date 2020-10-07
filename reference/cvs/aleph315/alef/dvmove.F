      SUBROUTINE DVMOVE (A,B,N)
C -----------------------------------------
C! Move a double precision array
C - F.Ranjard - 970220
C - Input   : A /DP  = double precision input array
C             N /I   = number of words to move
C - Output  : B /DP  = double precision output array
C ---------------------------------------------------
#ifndef DOC
      DOUBLE PRECISION A(*), B(*)
      INTEGER N
C ---------------------------------------------------
      DO I=1,N
         B(I) = A(I)
      ENDDO
C
      END
#endif
