      LOGICAL FUNCTION VTRLAS (DUM)
C----------------------------------------------------------------------
C!  - .TRUE. If VDET Laser Trigger
CKEY VDET LASER TRIGGER / USER
C!   Author   :- E. Lancon             30-JUN-1993
C!   modified : A.Wagner and F.Ranjard 12-APR-1995
C?
C!======================================================================
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JXTBID=1,JXTBVR=2,JXTBTN=4,JXTBTB=5,JXTBBM=6,JXTBBN=8,
     +          JXTBAD=18,JXTBL1=23,JXTBL2=24,LXTBNA=24)
      CHARACTER*4 MNM1,MNM2,CHAINT
      INTEGER AGETDB, ALTRIG, ALFIND
      SAVE INUM
      DATA NAXTBN, IRLAST /2*0/
C
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
C
      VTRLAS = .FALSE.
      IF (NAXTBN.EQ.0) NAXTBN = NAMIND('XTBN')
      CALL ABRUEV (IRUNRC,IEV)
C
C - new run ====================================
C
      IF (IRUNRC .NE. IRLAST) THEN
C      reset INUM to wrong value
         INUM = -1
         IRLAST = IRUNRC
C      IF XTBN is not on the run header list  OR
C         is not there THEN get it from DB
         IXTBN = ALFIND('C','XTBN')
         KXTBN  = IW(NAXTBN)
         IF (IXTBN.EQ.0 .OR. KXTBN.EQ.0) THEN
            IDUMM = AGETDB('XTBN',IRUNRC)
            KXTBN = IW(NAXTBN)
         ENDIF
         IF (KXTBN.NE.0) THEN
            DO I=1,LROWS(KXTBN)
C
C            is this trigger bit enabled?
C            exclude bit # 31
C
               IDEF=ITABL(KXTBN,I,JXTBTN)
               INUM=ITABL(KXTBN,I,JXTBTB)
C
C            For some of the early runs XTBN was corrupted
C
               IF(INUM.LT.0.OR.INUM.GT.31) GOTO 888
C
C            get mnemonic
C
               IF(IDEF.NE.0)THEN
                 MNM1=CHAINT(ITABL(KXTBN,I,JXTBBM))
                 MNM2=CHAINT(ITABL(KXTBN,I,JXTBBM+1))
                 IF(MNM1(1:4).EQ.'VDET'.AND.MNM2(1:4).EQ.'_LSR')GOTO 100
               ENDIF
            ENDDO
            GOTO 888
         ENDIF
C
      ENDIF
C
C - same run ==========================================
C
  100 CONTINUE

C?    IF trigger bit number and trigger bit pattern are OK
C     THEN compare trigger bit pattern with trigger bit number
C
      IF( INUM .GT. -1) THEN
        IF (ALTRIG(JLVL1,JLVL2,JLVL3).GT.0) VTRLAS = BTEST(JLVL3,INUM)
      ENDIF
C
C =====================================================
C
  888 RETURN
      END
