      SUBROUTINE EBCONF( STAC , KLAS )
C ----------------------------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C!  Calculate the configuration index.
CKEY PHOTONS GAMMA CONFIGURATION / INTERNAL
C   The 3 stacks are compared to fixed thresholds in order to find
C   the configuration index of the cluster.
C   A configuration class is defined by respect to the  following
C   thresholds :
C   Stack 1 :.050 Gev , Stack 2 :.070 Gev , Stack 3 :.100 Gev
C   IF( Stack(k) .GE. Threshold(k) ) Ik = 1
C                ELSE                Ik = 0
C                KLAS = I1 + 2 * I2 + 4 * I3
C   ---------------------------------------------------
C   KLAS    :    0    1    2    3    4    5    6    7
C   ---------------------------------------------------
C   Stack 1 :    -    +    -    +    -    +    -    +
C   Stack 2 :    -    -    +    +    -    -    +    +
C   Stack 3 :    -    -    -    -    +    +    +    +
C   ---------------------------------------------------
C
C   Input : STAC(3)     Stacks content in Gev.
C
C   Output: KLAS        Stack configuration index.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Called by EBPRGA  EBLPST
C-----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION STAC(*) , SEUY(3)
C   The thresholds were chosen from the test runs data analysis
C   and cannot be changed.
      DATA   SEUY / .05 , .07 , .10 /
C -------------------------------------------------------
      KLAS = 0
      DO 1 I = 1 , 3
      IF( STAC(I) .LT. SEUY(I) ) GO TO 1
      KLAS = KLAS + I
      IF( I .EQ. 3 ) KLAS = KLAS + 1
    1 CONTINUE
      RETURN
      END
#endif
