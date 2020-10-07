      SUBROUTINE OMBPOS (DAT, PEDS, GAIN, XYBOM, BCUR, IRET)
C----------------------------------------------------------------------
C!  - Calculate beam position and current at BOM pickup
C!
C!   Author:  R.W.Forty  23-May-91
C!
C!   Input:  DAT   = Raw data for each electrode
C!           PEDS  = Pedestals for each electrode
C!           GAIN  = Relative gain for electrode pairs
C!   Output: XYBOM = 4-component vector containing the positions
C!                   at the BOM pickups: (xA, xB, yA, yB)
C!           BCUR  = Beam current at each BOM (in ADC counts)
C!           IRET  = 0  OK
C!                >< 0  Error
C?
C!======================================================================
#ifndef DOC
#include "bomcal.h"
C
      REAL DAT(*), PEDS(*), GAIN(*), XYBOM(*), BCUR(*), PH(4),
     &     RG, P21, P43, Q21, Q43
      INTEGER IBOM, IRET, IOFF, IE
C.......................................................................
C
      DO 200 IBOM = 1, 2
        IOFF = 4 * (IBOM - 1)
        BCUR(IBOM) = 0.
C
        DO 100 IE = 1, 4
          RG = 1.
          IF (MOD(IE, 2) .EQ. 0) RG = GAIN( (IE + IOFF)/2 )
          PH(IE) = ABS( DAT(IE + IOFF) - PEDS(IE + IOFF) ) * RG
          BCUR(IBOM) = BCUR(IBOM) + PH(IE)
  100   CONTINUE
C
        P21 = PH(2) + PH(1)
        P43 = PH(4) + PH(3)
        IF (P21 .LE. 0.01 .OR. P43 .LE. 0.01) GOTO 998
C
        Q21 = (PH(2) - PH(1)) / P21
        Q43 = (PH(4) - PH(3)) / P43
C
        XYBOM(IBOM)     = PCALMM * (Q43 - Q21)
        XYBOM(IBOM + 2) = PCALMM * (Q21 + Q43)
C
  200 CONTINUE
C
      IRET = 0
      GOTO 999
C
  998 CALL RERROR ('OMBPOS', 1, ' Error calculating position at BOM')
      IRET = 1
C
  999 RETURN
      END
#endif
