      INTEGER FUNCTION JHOCHA (CHAR)
C -----------------------------------------------------------------
C - F.Ranjard - 890228
C! return in JHOCHA the hollerith representation of the character
C! variable CHAR
C  ----------------------------------------------------
      CHARACTER*4   CHAR
      CALL UCTOH (CHAR,IHOL,4,4)
      JHOCHA = IHOL
      END
