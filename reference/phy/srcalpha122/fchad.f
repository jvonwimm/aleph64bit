C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FCHAD(IND)
C ...............................................................
C . True if hadron IND is charmed                               .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IND
      LOGICAL FCQUA,FCBAR
      FCHAD = FCQUA(IND).OR.FCBAR(IND)
      RETURN
      END
