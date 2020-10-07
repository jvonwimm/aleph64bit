      REAL FUNCTION ALELEP( IRUN )
C --------------------------------------------------------------
C - F.Ranjard - 900803
C! Returns the LEP energy for a given run
CKEY ALEF LEP ENERGY
C - Input    : IRUN    / I     = run number
C
C Modified  M.Cattaneo   10/02/1997
C Original routine superseded by ALEFIL with new calling sequence
C ------------------------------------------------------------------
#ifndef DOC
C=========================================
C +
C Declarations.
C -
      IMPLICIT NONE

      INTEGER IRUN
      INTEGER JLFIL, IROW

      REAL ALEFIL
      EXTERNAL ALEFIL
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      ALELEP = ALEFIL( IRUN, JLFIL, IROW )
C
  999 RETURN
      END
#endif
