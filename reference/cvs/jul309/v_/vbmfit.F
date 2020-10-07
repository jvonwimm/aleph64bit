      SUBROUTINE VBMFIT (NEWROW)
C----------------------------------------------------------------------
C!  - Beamspot position fit
C!
C!   Author   :- D. Brwon                   8-APR-1993
C!  First written by G. Redlinger
C!
C!   Bug fix: Steve Wasserbaech, April 1995
C!   Use existing first-event-in-chunk number when NEWROW = .FALSE.
C!   Modified: SW, 18 October 1995
C!   Switch to VBPC; introduce track deweighting in first iteration;
C!   protect against failure in VCBFIT.
C?
C!======================================================================
#ifndef DOC
C     IMPLICIT NONE
      SAVE FIRST, NAVBPC
#include "bcs.h"
#include "rcurnt.h"
#include "vbpcjj.h"
#include "alpbjj.h"
#include "vbscom.h"
C
      INTEGER NAVBPC, KVBPC, NLOOP, ILOOP, IPAR, KALPB, LEN
      INTEGER IGARB, IP
      INTEGER NAMIND, NLINK
      REAL BPVEC(3), BPERR(3), BP(2)
      LOGICAL NEWROW, FIRST
      DATA FIRST / .TRUE. /
      EXTERNAL NLINK
C
#include "bmacrod.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NAVBPC = NAMIND('VBPC')
      ENDIF
      KVBPC = IW(NAVBPC)
      IF (KVBPC .LE. 0) GO TO 999
C
C  Make sure the VBPC row number (= LEP energy regime) is valid:
C
      IF (IVBPC .LE. 0) GO TO 999
C
C  Start by assuming a (0,0) beam position:
C
      BP(1) = 0.
      BP(2) = 0.
C
C  Iterate, trimming harder each time:
C
      NLOOP = ITABL(KVBPC,IVBPC,JVBPNI)
      DO ILOOP=1,NLOOP
        BSD0SCUT = RTABL(KVBPC,IVBPC,JVBPSC-1+ILOOP)
C
C  In the first iteration, deweight the tracks with small
C  d0 uncertainties, in case they are cosmics:
C
        IF (ILOOP .EQ. 1) THEN
          S2MIN = RTABL(KVBPC,IVBPC,JVBPSM)**2
        ELSE
          S2MIN = 0.
        ENDIF
C
C  Perform the fit:
C
        CALL VCBFIT(BP,BPVEC,BPERR)
C
C  If the fit failed, don't continue iterating:
C
        IF (BPERR(1) .EQ. 0.) GO TO 100
C
C  Update the beam position for the next fit:
C
        BP(1) = BPVEC(1)
        BP(2) = BPVEC(2)
      ENDDO
C
C  Scale the uncertainties:
C
      DO IPAR=1,2
        BPERR(IPAR) = BPERR(IPAR)*RTABL(KVBPC,IVBPC,JVBPKX-1+IPAR)
      ENDDO
C
C  Write these entries into a BOS bank.  Only create a new row
C  if requested.  If the row is just being updated, don't change
C  the starting event number.  ALPB is created the first time in
C  VBSINR, with first event number = 1.
C
 100  CONTINUE
      KALPB = NLINK('ALPB',IRUNRC)
      IF (KALPB .LE. 0) GO TO 820
C
      IF (NEWROW) THEN
        LEN = IW(KALPB) + LCOLS(KALPB)
        CALL AUBOS('ALPB',IRUNRC,LEN,KALPB,IGARB)
        IF (IGARB .EQ. 2) GO TO 800
        IP = KNEXT(KALPB)
        IW(KALPB+LMHROW) = IW(KALPB+LMHROW)+1
        IW(IP+JALPFE) = IBSFEV
      ELSE
        IP = KROW(KALPB,LROWS(KALPB))
      ENDIF
      IW(IP+JALPXP) = NINT(BPVEC(1)/BSPFAC)
      IW(IP+JALPXE) = NINT(BPERR(1)/BSEFAC)
      IW(IP+JALPYP) = NINT(BPVEC(2)/BSPFAC)
      IW(IP+JALPYE) = NINT(BPERR(2)/BSEFAC)
C
C  done
C
      GO TO 999
C
  800 CONTINUE
  810 CALL RERROR ('VBMFIT', -1, 'No Space to enlarge ALPB !!')
      GO TO 999
C
  820 CONTINUE
      CALL RERROR ('VMBFIT', -2, 'ALBP is NOT defined ')
      GO TO 999
C
  999 RETURN
      END
#endif
