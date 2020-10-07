C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FBQUA(IND)
C ...............................................................
C . True if hadron IND contains a b quark                       .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IHQUA,IND
      FBQUA = .FALSE.
      IF (IHQUA(IND).EQ.1) FBQUA = .TRUE.
      RETURN
      END
