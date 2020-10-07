C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FHQUA(IND)
C ...............................................................
C . True if hadron IND contains a b or a c quark                .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IND
      LOGICAL FBQUA,FCQUA
      FHQUA = FBQUA(IND).OR.FCQUA(IND)
      RETURN
      END
