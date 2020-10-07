      SUBROUTINE UBA2PI(PHI)
C! Bound an angle in the range 0 -> 2pi
C! Modified 2000-12-04  M.Cattaneo :
C!                      Bound angle by taking MOD, instead of DO loop!
#ifndef DOC
      DOUBLE PRECISION PHI
#include "uftcom.h"

      IF( PHI .LT. 0.D0) THEN
        PHI = TWO_PI + MOD( PHI, TWO_PI )
      ELSE IF( PHI .GT. TWO_PI ) THEN
        PHI = MOD( PHI, TWO_PI )
      ENDIF
C
      RETURN
C
      ENTRY UBAPI(PHI)
C! Bound an angle in the range -pi -> pi

      IF( PHI .LT. 0.D0) THEN
        PHI = TWO_PI + MOD( PHI, TWO_PI )
      ELSE IF( PHI .GT. TWO_PI ) THEN
        PHI = MOD( PHI, TWO_PI )
      ENDIF
      IF ( PHI .GT. ONE_PI ) PHI = PHI - TWO_PI

      RETURN
C
      ENTRY UBAPI2(PHI)
C! Bound an angle in the range -pi/2 -> pi/2

      IF ( PHI .GT. HALF_PI ) PHI = PHI - ONE_PI
      IF ( PHI .LT.-HALF_PI ) PHI = PHI + ONE_PI

      RETURN
C
      END
#endif
