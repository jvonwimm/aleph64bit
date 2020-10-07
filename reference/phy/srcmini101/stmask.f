      SUBROUTINE STMASK(IRUN,MASKL,MASKR,MASKP,MASKS,MASK)
C-------------------------------------------------------------
C!    Return trigger masks for current run
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
      SAVE
      DIMENSION MASK(3)
      CHARACTER*4 MNM1,MNM2,CHAINT
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CA XTBNJJ
      PARAMETER(JXTBID=1,JXTBVR=2,JXTBTN=4,JXTBTB=5,JXTBBM=6,JXTBBN=8,
     +          JXTBAD=18,JXTBL1=23,JXTBL2=24,LXTBNA=24)
C*CC XTBNJJ
      INTEGER AGETDB
      EXTERNAL AGETDB
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
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
    1   CONTINUE
        GOTO 999
      ENDIF
C
C     IF NO XTBN BANK IS FOUND ...  OR something was wrong
  888 CONTINUE
C        CALL RERROR('STMASK',-1,' XTBN bank problem')
  999 RETURN
      END
