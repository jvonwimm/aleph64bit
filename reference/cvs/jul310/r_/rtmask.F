      SUBROUTINE RTMASK(IRUN,MASKL,MASKR,MASKP,MASKS)
C-------------------------------------------------------------
C!    Return trigger masks for current run
C
C     INPUT:  IRUN
C     OUTPUT: MASKL - Trigger mask for any LCAL trigger (incl SATR)
C             MASKR - Trigger mask for random   trigger
C             MASKP - Trigger mask for any physics trigger
C             MASKS - Trigger mask for SATR coincidence trigger
C
C      Author J.Knobloch 9-MAR-1990
C!    Modified:   H. Meinhard       23-Aug-1990  (1)
C!                      Add SATR trigger bit
C!    Modified:   H. Meinhard       04-Jul-1991  (2)
C!                      Obtain XTBN from data base, if necessary
C?
C!======================================================================
#ifndef DOC
      CHARACTER*4 MNM1,MNM2,CHAINT
#include "bcs.h"
#include "xtbnjj.h"
      INTEGER AGETDB
      EXTERNAL AGETDB
#include "bmacro.h"
C---------------------------------------------------------------
      MASKL = 0
      MASKR = 0
      MASKP = 0
      MASKS = 0
      NAXTBN=NAMIND('XTBN')
      KXTBN=IW(NAXTBN)
C fetch XTBN from data base, if it does not exist in the run header
      IF (KXTBN .EQ. 0) THEN
        CALL RERROR('RTMASK',2,'XTBN not on SOR, try data base')
        IDUMM = AGETDB('XTBN',IRUN)
        KXTBN = IW(NAXTBN)
      ENDIF
      IF(KXTBN.NE.0)THEN
        NBIT=LROWS(KXTBN)
        DO 1 I=1,NBIT
C
C      is this trigger bit enabled?
C      exclude bit # 31
C
          IDEF=ITABL(KXTBN,I,JXTBTN)
          INUM=ITABL(KXTBN,I,JXTBTB)
C       For some of the early runs XTBN was corrupted
C
          IF(INUM.LT.0.OR.INUM.GT.31) THEN
            KXTBN=0
            GOTO 888
          ENDIF
          IF(IDEF.NE.0)THEN
C
C      get mnemonic
C
            MNM1=CHAINT(ITABL(KXTBN,I,JXTBBM))
            MNM2=CHAINT(ITABL(KXTBN,I,JXTBBM+1))
            IF(MNM1(1:4).NE.'LVL2')THEN
              IF(MNM1(1:1).EQ.'L')THEN
                MASKL=IBSET(MASKL,INUM)
                IF(MNM1(1:2).EQ.'LS') MASKS=IBSET(MASKS,INUM)
              ELSE IF(MNM1//MNM2.EQ.'RNDM_TRG')THEN
                MASKR=IBSET(MASKR,INUM)
              ELSE IF(MNM1(1:1).NE.' ')THEN
                MASKP=IBSET(MASKP,INUM)
              ENDIF
            ENDIF
          ENDIF
    1   CONTINUE
        GOTO 999
      ENDIF
C
C     IF NO XTBN BANK IS FOUND ...
C     OR somthing was wrong
  888 CONTINUE
      CALL RERROR('RTMASK',-1,' XTBN bank problem')
C-      This is Temporary and should be removed when XTBN is in DAF
      MASKR=IBSET(MASKR,31)
      MASKP=2052274884
      MASKS=268435458
      IF(IRUN.LT.4535)THEN
        MASKL=12632312
      ELSE
        MASKL=268550394
      ENDIF
  999 RETURN
      END
#endif
