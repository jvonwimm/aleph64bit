      FUNCTION MINHIT(IH,IDET)
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Find number of hits in MVD, ITC or TPC from DTRA bank.
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : IH     = hit pattern from DTRA bank
C              IDET   = 1, 2 or 3 for MVD, ITC or TPC
C     Output : MINHIT = number of hits observed in specified detector
C
C     The routine counts the number of bits in the part of the hit
C     pattern word (word-part) corresponding to the specified detector.
C     For reasons of speed, it uses the word-part (or 8-bit sub-parts)
C     as indices to an array K which contains the bit sums
C     corresponding to the addressing byte.
C     This is a factor 4 faster than doing the bit sums dirrectly, and
C     a factor 6 faster than using a logical bit test.
C     This method was suggested to me by David Asbury of DD.
C-----------------------------------------------------------------------
C
      COMMON / MINLOC / K(0:255)
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C
      IF(FIRST) THEN
        DO 100 I=0,255
 100    K(I) = JBIT(I,1) + JBIT(I,2) + JBIT(I,3) + JBIT(I,4) +
     &         JBIT(I,5) + JBIT(I,6) + JBIT(I,7) + JBIT(I,8)
        FIRST = .FALSE.
      ENDIF
C
      GOTO (1,2,3), IDET
   1  MINHIT = K(JBYT(IH, 1,2))
      RETURN
   2  MINHIT = K(JBYT(IH, 3,8))
      RETURN
   3  MINHIT = K(JBYT(IH,11,8)) + K(JBYT(IH,19,8)) + K(JBYT(IH,27,5))
      RETURN
C
      END
