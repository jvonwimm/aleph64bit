      SUBROUTINE XHVSTA(XHV)
CKEY XLUMOK TRIG /INTERNAL
C----------------------------------------------------------------------
C!   Check HV status bits
C!   Called from XLSLUM
C!   Author   :-       J.Wear            8-FEB-1990
C!    Modified:   H. Meinhard       27-Apr-1993  (1) - Add Sical
C!    Modified:   B. Bloch          November 97  adapted from QHVSTA in ALPHA
C!                                  to go to Alephlib
C!    Modified:   B. Bloch       October 98   call general fix routine XHVFIX
C!
C!   Inputs:           None
C!
C!   Outputs:          logical array XHV(24)
C!
C!   Libraries required:
C!
C!   Description:
C!     To be used within any ALEPH program
C!     Returns the High Voltage status of ALEPH according
C!     the bit mapping of X1RG or XTCN ( when reading Raw Data),
C!     REVH, word 1 (POT). Special care is taken to repair
C!     some run-specific cases where the REVH bits do not reflect the
C!     actual detector status.
C!    For DST-compatibility
C!      0) Conventional triggers  -- use REVH and LOLE
C!      1) Luminosity-only events -- use REVH and LUPA's copy of LOLE
C!        (for Lcal events )  or  SILH's copy of LOLE (for Sical events)
C!      2) Single Neutral EM-only -- XLUMOK is FALSE. (no LOLE bank)
C!     Monte Carlo runs are returned with .TRUE. (on) status.
C?
C!======================================================================
#ifndef DOC
      SAVE MTPHV,MTFEV,MTLEV
      SAVE FIRST,NALUPA,NASILH,NALOLE
#include "bcs.h"
#include "lupajj.h"
#include "silhjj.h"
#include "lolejj.h"
#include "xhvbit.h"
      LOGICAL XHV(NHVBIT)
      PARAMETER (NTVSEP=7)
      DIMENSION MTPHV(NTVSEP),MTFEV(NTVSEP),MTLEV(NTVSEP)
      LOGICAL BTEST
      LOGICAL FIRST /.TRUE./
      INTEGER  xhvbit
      EXTERNAL xhvbit
      DATA MTPHV /4316,4328,4350,4351,4357,4367,4371/
      DATA MTFEV /   1, 154,1320,   1, 121, 372,   1/
      DATA MTLEV /2080, 804,1576,9999,9999,9999, 251/
#include "bmacro.h"
C----------------------------------------------------------------------
C Save the name indices
      IF( FIRST ) THEN
        NALUPA = NAMIND('LUPA')
        NASILH = NAMIND('SILH')
        NALOLE = NAMIND('LOLE')
        FIRST = .FALSE.
      ENDIF
C
      CALL abruev(krun,kevt)
C
C For Monte Carlo (KRUN < 2000), all subdetectors are on
      IF (KRUN.LE.2000) THEN
        DO 1 IBIT = 1,NHVBIT
    1   XHV(IBIT) = .TRUE.
        GOTO 999
      ENDIF
C
C Get detector HV status independent of detector or run specific repairs
      iret = XHVBIT(jhvst)
      DO 10 IBIT = 1,NHVBIT
        XHV(IBIT) = BTEST(JHVST,(IBIT-1))
   10 CONTINUE
C
C Now detector-specific, run-specific repair to HV bits
C
C Vertex Detector
C
C
C ITC
C
C Message by Bill Cameron 11-Jun-1993: All events in run 20600 after event
C 2787 should be discarded.
      IF (KRUN .EQ. 20600) THEN
        IF (KEVT .GE. 2787) XHV(JITC) = .FALSE.
      END IF
C
C
C TPC
C  1) Both TPC logicals are identically set for Sept 89 runs.
C  2) Sept `89: 7 runs had TPC HV bits which should be ignored
C               and are turned HV on here for the proper events.
C
      IF (KRUN.LE.4800) THEN  ! September 89 run corrections.
        DO 20 IBAD = 1,NTVSEP
          IF (KRUN.EQ.MTPHV(IBAD)) THEN
            XHV(JTEX) = .FALSE.
            IF ((KEVT.GE.MTFEV(IBAD)).AND.(KEVT.LE.MTLEV(IBAD))) THEN
              XHV(JTEX) = .TRUE.
              GOTO 21
            ENDIF
          ENDIF
   20   CONTINUE
   21   XHV(JTTR) = XHV(JTEX)
      ENDIF
C
C ECAL
C
C
C HCAL
C 1) Turn on the HCAL for Sept.89 (before HV bit existed)
      IF (KRUN.LE.4800) THEN
        XHV(JHEA) = .TRUE.
        XHV(JHEB) = .TRUE.
        XHV(JHBR) = .TRUE.
      ENDIF
C 2) Run 20639: endcaps not at nominal voltage at the beginning of the
C    run -> lower efficiency
      IF (KRUN .EQ. 20639) THEN
        IF (KEVT .LT. 690) THEN
          XHV(JHEA) = .FALSE.
          XHV(JHEB) = .FALSE.
        END IF
      END IF
C 3) Run 23276: HCAL not operational for part of the run
      IF (KRUN .EQ. 23276) THEN
        IF (KEVT .GT. 11459) THEN
          XHV(JHEA) = .FALSE.
          XHV(JHEB) = .FALSE.
          XHV(JHBR) = .FALSE.
        END IF
      END IF
C
C
C Muon chambers
C
C
C LCAL
C Check the LOLE (or LUPA) bank for errors also
C 1) Completely ignore LOLE bank for run 4107
C 2) Completely ignore REVH bit for run 4530
C 3) Two Oct.`89 runs with erroneous LOLE banks
      LOLEWD = 1
      KLUPA = IW(NALUPA)
      KLOLE = IW(NALOLE)
      KSILH = IW(NASILH)
      IF (KLUPA.GT.0) THEN
        LOLEWD = ITABL(KLUPA,1,JLUPLO)
      ELSEIF (KLOLE.GT.0) THEN
        LOLEWD = ITABL(KLOLE,1,JLOLER)
      ELSEIF (KSILH.GT.0) THEN
        LOLEWD = ITABL(KSILH,1,JSILLO)
      ENDIF
C
      IF(KRUN.EQ.4107)  LOLEWD = 0          ! Sept.89 running exceptions
      IF(KRUN.EQ.4530)  XHV(JLCAL)=.TRUE.
      IF(KRUN.EQ.4981.OR.KRUN.EQ.4982) LOLEWD = 0 ! Oct.`89 running exce
      IF(KRUN.GE.10606.AND.KRUN.LE.10611) LOLEWD = 0 ! May.`91 running e
      XHV(JLCAL) = XHV(JLCAL).AND.(LOLEWD.EQ.0)
C
C
C SICAL
C
C   1) Fill 1451 (15/16-Nov-1992) had two amplexes which went bad. Take
C      that into account by resetting the HV bit.
      IF (KRUN .GE. 17766 .AND. KRUN .LE. 17781) XHV(JSCAL) = .FALSE.
C   2) Fill 1533 (18    May-1993) had some low voltage problem|
C      Fill 1574 (30    May-1993) had some low voltage problem| part of
C      Fill 1577 (31    May-1993) had some low voltage problem| some run
C
      IF ((KRUN .EQ. 20170).AND.(KEVT .GE.  3700)) XHV(JSCAL) = .FALSE.
      IF ((KRUN .EQ. 20592).AND.(KEVT .LE.  1500)) XHV(JSCAL) = .FALSE.
      IF ((KRUN .EQ. 20619).AND.(KEVT .GE.  7200)) XHV(JSCAL) = .FALSE.
      IF ((KRUN .EQ. 20621).AND.(KEVT .LE.   500)) XHV(JSCAL) = .FALSE.
      IF ( KRUN .EQ. 20620                      )  XHV(JSCAL) = .FALSE.
C
C
C SATR
C
C
C BCAL
C
C
C Trigger
C
C 1) Turn on the trigger analog/CAMAC crate low voltages for Sept.89
C (before HV bit existed)
      IF (KRUN.LE.4800) THEN
        XHV(JTGA) = .TRUE.
        XHV(JTGC) = .TRUE.
      ENDIF
C
      CALL XHVFIX(XHV)
C
  999 CONTINUE
      END
#endif
