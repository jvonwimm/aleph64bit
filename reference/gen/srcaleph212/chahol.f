      CHARACTER*4 FUNCTION CHAHOL(IHOL)
C --------------------------------------------------------------------
C - F.Ranjard - 890301
C! return in CHAHOL the character representation of the hollerith IHOL
C ----------------------------------------------------------
      CHARACTER*4 CHAHO
      CALL UHTOC (IHOL,4,CHAHO,4)
      CHAHOL = CHAHO
      END
