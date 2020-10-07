      LOGICAL FUNCTION FBBAR(IND)
C ...............................................................
C . True if hadron IND contains a b-bar quark                   .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IHQUA,IND
      FBBAR = .FALSE.
      IF (IHQUA(IND).EQ.-1) FBBAR = .TRUE.
      RETURN
      END
