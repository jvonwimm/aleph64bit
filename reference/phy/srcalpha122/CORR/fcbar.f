C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FCBAR(IND)
C ...............................................................
C . True if hadron IND contains a c-bar quark                   .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IHQUA,IND
      FCBAR = .FALSE.
      IF (IHQUA(IND).EQ.-2) FCBAR = .TRUE.
      RETURN
      END
