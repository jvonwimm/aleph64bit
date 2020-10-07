*DK tdebug
      SUBROUTINE TDEBUG (NEVT)
C----------------------------------------------------------------------
C
C    Created by Doug Cowen         9-AUG-1988
C
C    Purpose:
C!               Make output necessary for the TPCSIM stand-alone
C!               version to work on problematic events.  GALEPH does
C!               not reproduce random number sequence, but TPCSIM does
C!               so prior to each GALEPH call to TGTEER, we call this
C!               routine (if LDEBUG .EQ. .TRUE.) which outputs the
C!               starting random number seed, and the RUNH, EVEH and
C!               TPTE banks.  The banks are written out to a file
C!               which is overwritten for each event; the random number
C!               seed appears in the log file.
C!   Inputs    : LDEBUG flag, BOS banks RUNH, EVEH, TPTE.
C                NEVT / I  = event number
C!   Outputs   : BOS banks RUNH, EVEH, TPTE.  Random number seed at
C!               start of event.
C!
C!   Called by : TGTEER
C    Calls    : RDMOUT (CERNLIB), BWRITE, VMCMS (IBM only), ALEVEH
C               ALTELL (ALEPHLIB)
C
C    Modifications :
C     1.- P. Janot  06 sep 88 -- Allow IBM users to use TDEBUG !
C     2.- P. Janot  01 oct 88 -- Build event header if not there.
C     3.- D. Cowen  17 oct 88 -- Get NAEVEH from BOS call, not from
C                                GALEPH commons, since these commons
C                                do not exist for standalone TPCSIM.
C     4.- F.Ranjard 28 mar 89 -- suppress references to GALEPH *CD and
C                                add argument in the calling sequence.
C                                adapt to new random number generators.
C!
C----------------------------------------------------------------------
C
#include "jqcom.h"
#include "tpcbos.h"
#include "tpcond.h"
#include "faster.h"
C
      CHARACTER*80 COM
      INTEGER ALEVEH,DBGUNI,RNDSED(3)
C
      DATA DBGUNI, ICALL / 75, 0/
#if defined(VAX)
      CALL BUNIT(DBGUNI,'FORT',16020)
      OPEN(UNIT=DBGUNI,FORM='UNFORMATTED',STATUS='UNKNOWN',ERR=888)
#endif
#if defined(IBM)
      IF(ICALL.EQ.0) THEN
        COM='FI IOFILE75 DISK TPCDBG EPIO A(RECFM F BLKSIZE 32040 PERM'
        CALL VMCMS(COM,IRC)
        IF(IRC .GT. 0) GOTO 888
        CALL BUNIT(DBGUNI,'EPIO',16020)
        ICALL = 1
      ENDIF
      CALL BRWND(DBGUNI)
#endif
#if defined(APOLLO)
      CALL BUNIT(DBGUNI,'FORT',16020)
      OPEN(UNIT=DBGUNI,FILE='tdebug',FORM='UNFORMATTED',STATUS='UNKNOWN'
     +     ,ERR=888)
#endif
C
      NAEVEH  = NAMIND('EVEH')
      KEVEH   = IW(NAEVEH)
C
C  Build the 'EVEH' if not there
C
      IF(KEVEH .EQ. 0) THEN
        ISTAT = 1
        IEXP  = 1001
        ECMS  = 90.
        IPRO  = 1001
        IRUN  = 1001
        KRUNH = IW(NAMIND('RUNH'))
        IF (KRUNH.GT.0) THEN
           IEXP = IW(KRUNH+1)
           IRUN = IW(KRUNH+2)
           IPRO = IW(KRUNH+3)
        ENDIF
        KEVEH = ALEVEH (NEVT,IEXP,IRUN,ECMS,IPRO,ISTAT)
      ENDIF
      IF (KEVEH.NE.0) THEN
         IEVENT = IW(KEVEH + 6)
C
         CALL BWRITE(IW,DBGUNI,'RUNHAFIDTSIM')
         CALL BWRITE(IW,DBGUNI,'EVEHTPTE')
         CALL BWRITE(IW,DBGUNI,'0')
C
         CALL RDMOUT(RNDSED)
         WRITE(6,100) IEVENT,RNDSED
C
      ELSE
         WRITE(6,101)
      ENDIF
C
      GO TO 999
C
  888 WRITE(6,102) DBGUNI
C
#if defined(VAX)
999   CLOSE(DBGUNI)
#else
999   CONTINUE
#endif
      RETURN
C
  100 FORMAT(/' TPCSIM DEBUG: seed at start of EVEH event ',I6,
     &         ' is : ',3I20/)
  101 FORMAT(/' +++DEBUG+++ No EVEH bank found.'//)
  102 FORMAT(/' +++DEBUG+++ Problem opening unit ',I2,
     &        ' for debug write.'//)
C
      END
