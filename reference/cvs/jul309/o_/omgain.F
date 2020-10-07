      SUBROUTINE OMGAIN (IDPA, DAT, PEDS, CALI, GAIN, GRAT, IRET)
C----------------------------------------------------------------------
C!  - Update relative gains for each BOM electrode pair
C!
C!   Author:  R.W.Forty  23-May-91
C!
C!   Input:  DAT   = Raw signals for each electrode
C!           PEDS  = Pedestals for each electrode
C!           CALI  = Calibration signal for each electrode
C!           IDPA  = 1  e+
C!                   2  e-
C!   Output: GAIN  = Relative gain for the 4 electrode pairs
C!           GRAT  = Gain ratio from calibration pulses for each
C!                   electrode pair
C!           IRET  = 0  OK
C!                >< 0  Error
C?
C!======================================================================
#ifndef DOC
#include "bomcal.h"
C
      REAL CALI(*), GAIN(*), GRAT(*), DAT(*), PEDS(*), PHEP
      INTEGER IEP, IEL, IDPA, IRET
C.......................................................................
C
      DO 100 IEP = 1, 4
        IEL = 2 * (IEP-1) + 1
        PHEP = ABS( DAT(IEL)-PEDS(IEL) + DAT(IEL+1)-PEDS(IEL+1) )
        IF (PHEP .LT. 0.01) GOTO 998
C
        GAIN(IEP) = GAINA0(IEP,IDPA) +
     &              GAINA1(IEP,IDPA) * PHEP +
     &              GAINA2(IEP,IDPA) * PHEP**2
C
C  Calculate gain ratio from calibration pulses
C
        IF ( ABS( CALI(2*IEP) - PEDS(2*IEP) ) .LT. 0.01 ) THEN
          GRAT(IEP) = 999.
        ELSE
          GRAT(IEP) = ( CALI(2*IEP-1) - PEDS(2*IEP-1) ) /
     &                ( CALI(2*IEP)   - PEDS(2*IEP)   )
        ENDIF
  100 CONTINUE
C
      IRET = 0
      GOTO 999
C
  998 CALL RERROR ('OMGAIN', 1, ' Bad pulse height for gain calc')
      IRET = 1
C
  999 RETURN
      END
#endif
