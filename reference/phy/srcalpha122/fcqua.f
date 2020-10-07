C
C---------------------------------------------------------------------
      LOGICAL FUNCTION FCQUA(IND)
C ...............................................................
C . True if hadron IND contains a c quark                       .
C .   IND    / I   Alpha index of the hadron                    .
C ...............................................................
      IMPLICIT NONE
      INTEGER IHQUA,IND
      FCQUA = .FALSE.
      IF (IHQUA(IND).EQ.2) FCQUA = .TRUE.
      RETURN
      END
