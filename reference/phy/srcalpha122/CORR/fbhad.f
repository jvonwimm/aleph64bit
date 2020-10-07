C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FBHAD(IND)
C ...............................................................
C . True if hadron IND is bottomed                              .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IND
      LOGICAL FBQUA,FBBAR
      FBHAD = FBQUA(IND).OR.FBBAR(IND)
      RETURN
      END
