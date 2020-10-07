      LOGICAL FUNCTION XT0SYN(KRUN)
CKEY  XLUMOK TRIG/INTERNAL
C----------------------------------------------------------------------
C! Get t0 synchronization information
C! Called from XLSLUM
C!    Author:     H. Meinhard       27-Apr-1993
C!
C!    Input:      - banks XTBN, XTRB
C!    Output:     - XT0SYN  /L      trigger synchronized with beam-X
C!
C----------------------------------------------------------------------
#ifndef DOC
      INTEGER NAXTBN / 0 /
      SAVE    LRUN, MSKT0, NAXTBN, FIRST
#include "bcs.h"
#include "xtbnjj.h"
C MFRRN = first run for which t0 synch bit worked
      PARAMETER (MFRRN=17061)
C MFR93 = first run for which t0 synch bit worked in 1993
      PARAMETER (MFR93=20000)
      CHARACTER*4 CHAINT
      CHARACTER*8 MNM
      CHARACTER*8 TTMSK
      LOGICAL XT0
      INTEGER ALTRIG
      EXTERNAL ALTRIG
C.....Trigger bit name
      DATA TTMSK / 'T0_SYNCH' /
C.....Hardwired trigger mask: in 92: bit 1 = Z00000002,
C     in 1993: bit 28 = Z10000000
      DATA MSKHD / 2 /, MSK93 / 268435456 /
      DATA LRUN /-9999/
#include "bmacro.h"
C----------------------------------------------------------------------
      XT0 = .TRUE.
C For MC, always return TRUE
      IF (KRUN .LT. 2000) GO TO 999
C Get the t0 synchronization bit mask, if new run
      IF (KRUN .NE. LRUN) THEN
        MSKT0 = 0
        IERR  = 0
C link to trigger bit name bank
        IF( NAXTBN .EQ. 0 ) NAXTBN = NAMIND('XTBN')
        KXTBN = IW(NAXTBN)
C scan the XTBN bank for the required triggers
        IF (KXTBN .NE. 0) THEN
          DO 300 ITBIT = 1, LROWS(KXTBN)
            IDEF = ITABL(KXTBN,ITBIT,JXTBTN)
            INUM = ITABL(KXTBN,ITBIT,JXTBTB)
C XTBN corrupted?
            IF (INUM .LT. 0 .OR. INUM .GT. 31) THEN
              IERR = 1
              GO TO 888
            END IF
C get the name of this trigger; convert it to upper case; check whether
C non-blank
            IF (IDEF .NE. 0 .AND. INUM .NE. 31) THEN
              MNM(1:4) = CHAINT(ITABL(KXTBN,ITBIT,JXTBBM))
              MNM(5:8) = CHAINT(ITABL(KXTBN,ITBIT,JXTBBM+1))
              CALL CLTOU(MNM)
              IF (MNM .NE. '        ') THEN
C compare with the required trigger names
                IF (MNM .EQ. TTMSK) MSKT0 = IBSET(MSKT0,INUM)
              END IF
            END IF
  300     CONTINUE
        ELSE
C XTBN is missing!
          IERR = 1
          GO TO 888
        END IF
C Error handling: XTBN missing or corrupted - use hardwired masks
  888   CONTINUE
        IF (IERR .NE. 0) THEN
          IF (KRUN .GT. MFRRN) MSKT0 = MSKHD
          IF (KRUN .GT. MFR93) MSKT0 = MSK93
        END IF
C Save the number of the run just initialized
        LRUN = KRUN
      END IF
C Caution! The meaning of the bit changed.
C In 1992: Bit on = synchronization of trigger with GBX is okay
C In 1993: Bit on = synchronization of trigger with GBX is BAD!
      IF (MSKT0 .NE. 0) THEN
        itrg = altrig(kxtet1,kxtet2,kxtel2)
        IF (KRUN .LT. MFR93) THEN
          XT0 = IAND(MSKT0,KXTET1) .EQ. MSKT0
        ELSE
          XT0 = IAND(MSKT0,KXTET1) .EQ. 0
        END IF
      END IF
  999 CONTINUE
      XT0SYN = XT0
      END
#endif
