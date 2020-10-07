C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FHVFL(IND)
C ...............................................................
C . True if hadron IND is heavy-flavoured                       .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IND
      LOGICAL FBHAD,FCHAD
      FHVFL = FBHAD(IND).OR.FCHAD(IND)
      RETURN
      END
