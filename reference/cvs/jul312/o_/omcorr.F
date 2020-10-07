      SUBROUTINE OMCORR
C----------------------------------------------------------------------
C!  - Calculate BOM corrector magnet deflections
C!
C!   Author:  R.W.Forty  16-Oct-91
C?
C!======================================================================
C
C June 7, 1996: O. Schneider: Increase LEP beam energy upper limit from 
C                             50 GeV to 110 GeV
#ifndef DOC
#include "bcs.h"
#include "bomflg.h"
#include "bomopt.h"
#include "bomcal.h"
C
      INTEGER I, KAB, KC
      REAL ELEP, ELEP0
C
C  LEP beam energy on peak
      DATA ELEP0 /45.625/
#include "bmacro.h"
C.......................................................................
C
C  Find run number
C
      KRUNR = IW( NAMIND('RUNR') )
      IF (KRUNR .EQ. 0) THEN
        CALL RERROR ('OMCORR', 1, ' Could not find run number')
        ELEP = ELEP0
      ELSE
        IRUN = IW(KRUNR + 2)
C
C  Find LEP beam energy
C
        ELEP = ALELEP (IRUN) / 2.
C
        IF (ELEP .LT. 40. .OR. ELEP .GT. 110.) THEN
          CALL RERROR ('OMCORR', 2, ' Bad LEP beam energy')
          ELEP = ELEP0
        ENDIF
      ENDIF
C
      IMCURR = 0
      DO 200 KAB = 1, 2
        DO 100 KC = 1, NUMCOR
          I = IADCOR(KC,KAB)
          DEFCOR(KC,KAB) = CURCOR(I) * CALCOR(KC,KAB) * 20. /
     &                     (ELEP * CSCALE)
          IMCURR = IMCURR + INT (10000. * DEFCOR(KC,KAB))
  100   CONTINUE
  200 CONTINUE
C
      RETURN
      END
#endif
