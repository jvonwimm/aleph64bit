      SUBROUTINE UBA2PI(PHI)
C! Bound an angle in the range 0 -> 2pi
#ifndef DOC
      DOUBLE PRECISION PHI
#include "uftcom.h"

      DO WHILE(PHI.LT.0.D0)
        PHI = PHI + TWO_PI
      ENDDO
      DO WHILE(PHI.GT.TWO_PI)
        PHI = PHI - TWO_PI
      ENDDO
C
      RETURN
C
      ENTRY UBAPI(PHI)
C! Bound an angle in the range -pi -> pi

      DO WHILE(PHI.LT.0.D0)
        PHI = PHI + TWO_PI
      ENDDO
      DO WHILE(PHI.GT.TWO_PI)
        PHI = PHI - TWO_PI
      ENDDO
      IF ( PHI .GT. ONE_PI ) PHI = PHI - TWO_PI
      IF ( PHI .LT.-ONE_PI ) PHI = PHI + TWO_PI

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
