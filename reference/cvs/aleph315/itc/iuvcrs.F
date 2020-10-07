      SUBROUTINE IUVCRS (A,B,C,IERR)
C----------------------------------------------------------------------
CKEY ITC
C! Calculates the vector cross product C = A X B
C
C IERR is error return flag:
C     0  ok
C     1  one of the input vectors has zero length
C     2  both input vectors are non-zero but are parallell;
C        output vector has zero length
C
C ARGUMENTS:
C ----------
C   A(1)                vector one
C   B(1)                vector two
C   C(1)                cross product of vector one with vector two
C   IERR                error return flag
C
C----------------------------------------------------------------------
C KEYWORDS: VECTOR CROSS PRODUCT
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
      REAL A(*), B(*), C(*)
      INTEGER IERR
C
      REAL ALEN, BLEN, CLEN
      INTEGER ICRS(4), I, I1, I2
      DATA ICRS /2,3,1,2/
C-------------------------------------------------------------
      IERR = 0
      ALEN=0.
      BLEN=0.
      CLEN=0.
      DO 21 I=1,3
         I1 = ICRS(I)
         I2 = ICRS(I+1)
         C(I) = A(I1)*B(I2) - A(I2)*B(I1)
         ALEN = ALEN + A(I)**2
         BLEN = BLEN + B(I)**2
         CLEN = CLEN + C(I)**2
   21 CONTINUE
C
      IF (CLEN .EQ. 0.) THEN
         IERR = 2
      ENDIF
      IF (ALEN .EQ. 0. .OR. BLEN .EQ. 0.) THEN
         IERR = 1
      ENDIF
      IF (IERR .GT. 0) THEN
         DO 31 I=1,3
            C(I) = 0.0
   31    CONTINUE
      ENDIF
C
      END
#endif
