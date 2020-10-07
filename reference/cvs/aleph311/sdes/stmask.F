      SUBROUTINE STMASK(IRUN,MASKL,MASKR,MASKP,MASKS,MASK)
C.-------------------------------------------------------------
CKEY SCALDES TRIGGER MASK
C!    Return trigger masks for current run using XTBN bank
C
C     INPUT:  IRUN
C     OUTPUT: MASKL - Trigger mask for any LCAL trigger
C             MASKR - Trigger mask for random   trigger
C             MASKP - Trigger mask for any physics trigger
C             MASKS - Trigger mask for SICAL  trigger
C
C      Author B Bloch-Devaux May 1992
C?
C!======================================================================
#ifndef DOC
      SAVE
      DIMENSION MASK(3)
      CHARACTER*4 MNM1,MNM2,CHAINT
#include "bcs.h"
#include "xtbnjj.h"
        INTEGER AGETDB
        EXTERNAL AGETDB, NAMIND
#include "bmacro.h"
C---------------------------------------------------------------
      MASKL = 0
      MASKR = 0
      MASKP = 0
      MASKS = 0
      CALL VZERO(MASK,3)
      NAXTBN=NAMIND('XTBN')
      KXTBN=IW(NAXTBN)
C fetch XTBN from data base, if it does not exist in the run header
        IF (KXTBN .EQ. 0) THEN
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
              IF(MNM1(1:2).EQ.'Si' .OR. MNM1(1:2).EQ.'SI')THEN
                 MASKS=IBSET(MASKS,INUM)
                 IF (MNM2(3:4).EQ.'LO') THEN
                    MASK(1)=IBSET(MASK(1),INUM)
                 ELSE IF (MNM2(3:4).EQ.'ME') THEN
                    MASK(2)=IBSET(MASK(2),INUM)
                 ELSE IF (MNM2(3:4).EQ.'HI') THEN
                    MASK(3)=IBSET(MASK(3),INUM)
                 ENDIF
              ELSE IF(MNM1(1:1).EQ.'L')THEN
                 MASKL=IBSET(MASKL,INUM)
              ELSE IF(MNM1//MNM2.EQ.'RNDM_TRG')THEN
                 MASKR=IBSET(MASKR,INUM)
              ELSE IF(MNM1(1:1).NE.' ')THEN
                 MASKP=IBSET(MASKP,INUM)
              ENDIF
           ENDIF
         ENDIF
   1     CONTINUE
        GOTO 999
      ENDIF
C
C     IF NO XTBN BANK IS FOUND ...  OR something was wrong
 888  CONTINUE
C        CALL RERROR('STMASK',-1,' XTBN bank problem')
 999  RETURN
      END
#endif
