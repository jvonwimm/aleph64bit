      SUBROUTINE OMREAD (IDPA, IDBU, DAT, SIG, PEDS, CALI, IRET)
C----------------------------------------------------------------------
C!  - Read BOM data from banks
C!
C!   Author:  R.W.Forty  23-May-91
C!   Modified by : J. Nash 8-2-91  Fixed e- and e+ (e- is first in BOMR)
C!   Input:  IDPA  = 1  e+
C!                   2  e-
C!   Output: IDBU  = Bunch id (1-8)
C!           DAT   = Raw signals for each electrode
C!           SIG   = Sigma (rms) of raw data measurement
C!           PEDS  = Pedestals for each electrode
C!           CALI  = Calibration signal for each electrode
C!           IRET  = 0  OK
C!                >< 0  Error
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "bomcal.h"
#include "bomrjj.h"
#include "bompjj.h"
C
      REAL DAT(*), SIG(*), PEDS(*), CALI(*)
      INTEGER I, IDPA, IDBU, IRET, KBOMR, KBOMP
#include "bmacro.h"
C.......................................................................
      IRET = 3
C
      KBOMR = IW( NAMIND('BOMR') )
      KBOMP = IW( NAMIND('BOMP') )
C
      IF (KBOMR .EQ. 0 .OR. KBOMP .EQ. 0) GOTO 997
C
C  Read pedestals
C
      DO 100 I = 1, 8
        PEDS(I) = FLOAT (ITABL (KBOMR, I, JBOMPE)) / DSCALE
        IF (PEDS(I) .LT. 1.) GOTO 998
  100 CONTINUE
C
C  Read data
C
      IF (IDPA .EQ. 2) THEN
        IDBU = ITABL (KBOMP, 1, JBOMBI)
        IF (IDBU .LT. 1 .OR. IDBU .GT. 8) GOTO 998
      ELSE
        IDBU = ITABL (KBOMP, 2, JBOMBI)
        IF (IDBU .LT. 1 .OR. IDBU .GT. 8) GOTO 998
      ENDIF
C
      IOFF = 8
      IF (IDPA .EQ. 2) IOFF = 0
      DO 200 I = 1, 8
        CALI(I) = FLOAT (ITABL (KBOMR, I+IOFF, JBOMCL)) / DSCALE
  200 CONTINUE
C
      DO 300 I = 1, 8
        DAT(I) = FLOAT (ITABL (KBOMR, I+IOFF, JBOMMN)) / DSCALE
        SIG(I) = FLOAT (ITABL (KBOMR, I+IOFF, JBOMSI)) / DSCALE
        IF (IDPA .EQ. 2) THEN
          IF (DAT(I) .LT. PEDS(I)+0.01) GOTO 998
        ELSE
          IF (DAT(I) .GT. PEDS(I)-0.01 .OR.
     &        DAT(I) .LT.         0.01) GOTO 998
        ENDIF
  300 CONTINUE
c
      IRET = 0
      GOTO 999
C
  997 CALL RERROR ('OMREAD', 1, ' Error reading BOM raw data banks')
      IRET = 1
      GOTO 999
C  998 CALL RERROR ('OMREAD', 2, ' Bad BOM raw data')
  998 CONTINUE
      IRET = 2
C
  999 RETURN
      END
#endif
