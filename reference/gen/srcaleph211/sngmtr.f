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
      SAVE
C --
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
C --
      LOGICAL TRIGMU(2),BTEST
      INTEGER ALTRIG
      CHARACTER*4 CHAINT
      CHARACTER*8 BNAME
      DATA ICOUNT/0/
C --
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
          CALL ALGTDB(LDBASE,'XTBN',IRUN)
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
