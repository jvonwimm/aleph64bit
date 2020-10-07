      SUBROUTINE SNGMTR(TRIGMU)
C----------------------------------------------------------------------
CKEY EDIR MUON TRIGGER
C! Single muon trigger test.
C-
C   Input  : None
C   Output : TRIGMU(1) : If true random trigger is available
C            TRIGMU(2) : If true single muon trigger is available
C-
C   Called by   : SNGRND
C   Calls  : None
C   Input banks : EVEH,XTBN
C-
C                                        Author: M. Talby September 89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "evehjj.h"
C --
      LOGICAL TRIGMU(2),BTEST
      INTEGER ALTRIG
      INTEGER ALGTDB,IRET
      CHARACTER*4 CHAINT
      CHARACTER*8 BNAME
      DATA ICOUNT/0/
C --
#include "bmacro.h"
C --
        TRIGMU(1) = .FALSE.
        TRIGMU(2) = .FALSE.
C --
      IRUN = -1
      KEVEH = IW(NAMIND('EVEH'))
      IF(KEVEH.LE.0) GOTO 999
C --
      IRUN = IW(KEVEH+JEVERN)
C --
      IF(IRUN.GT.4540) THEN
        KXTBN=IW(NAMIND('XTBN'))
        IF(KXTBN.LE.0) THEN
          LDBASE = JUNIDB(0)
          IRET = ALGTDB(LDBASE,'XTBN',IRUN)
          KXTBN=IW(NAMIND('XTBN'))
          IF(KXTBN.LE.0) GOTO 999
        ENDIF
C --
        IRNDMB = -1
        ISNGMB = -1
        NBITS = LROWS(KXTBN)
        DO 10 IBIT = 1,NBITS
          BNAME=CHAINT(ITABL(KXTBN,IBIT,6))//CHAINT(ITABL(KXTBN,IBIT,7))
          JBIT=ITABL(KXTBN,IBIT,5)
          IF(BNAME.EQ.'RNDM_TRG') THEN
            IRNDMB = JBIT
          ELSEIF(BNAME.EQ.'SNG_MUON') THEN
            ISNGMB = JBIT
          ENDIF
   10   CONTINUE
C --
        IF(IRNDMB.EQ.-1 .OR. ISNGMB.EQ.-1) THEN
          ICOUNT = ICOUNT+1
          IF(ICOUNT.LE.5) THEN
            IF(IW(6).GT.0) WRITE(IW(6),*) 'Warning SNGRND_ XTBN problem'
     &                    ,'Warning switched off after 5 prints'
          ENDIF
        ENDIF
      ELSE
        IRNDMB = 1
        ISNGMB = 8
      ENDIF
      ICODE = ALTRIG(IBITT1,IBITT2,IBITT3)
      IF(ICODE.EQ.0) GOTO 999
C --
C   Single muon and random trigger bits test
C --
      ITRG = IAND(IBITT1,IBITT3)
      IF(IRNDMB.GE.0) THEN
        IF(BTEST(ITRG,IRNDMB)) TRIGMU(1) = .TRUE.
      ENDIF
      IF(ISNGMB.GT.0) THEN
        IF(BTEST(ITRG,ISNGMB)) TRIGMU(2) = .TRUE.
      ENDIF
C --
  999 RETURN
      END
#endif
