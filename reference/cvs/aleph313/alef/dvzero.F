      SUBROUTINE DVZERO (ARRAY,N)
C ----------------------------------------------
CKEY ALEF ZERO DOUBLE PRECISION
C! set a double precision array to zero
C - F.Ranjard - 970218
C
C - Input : ARRAY / DP = double precision array
C           N     / I  = number of wordsto reset
C
C ------------------------------------------------
#ifndef DOC
      DOUBLE PRECISION ARRAY(*)
      INTEGER N, i
C
C --------------------------------------------------
C
      DO i =1,N
         ARRAY(i) = 0.D0
      ENDDO
C
      RETURN
      END
#endif
      
