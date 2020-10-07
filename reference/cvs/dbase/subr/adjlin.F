      SUBROUTINE ADJLIN(TEXT)
      CHARACTER TEXT*(*)
      COMMON /SLATE/ ND,NE,NF,DUMMY(36)
C     ND = # of characters in the word
C     NE = position of 1st blank after the word
C     JX = position of the 1st character of the word
C
      LE = LEN(TEXT)
      ND = 0
      JN = 0
 10   JN = JN+ND+1
      NX = LNBLNK(TEXT)
      IF (JN.LE.NX) THEN
         JX = ICNEXT(TEXT,JN,NX)
         TEXT(JN:LE) = TEXT(JX:LE)
         GOTO 10
      ENDIF
      RETURN
      END
