      INTEGER FUNCTION CAFIHT (NCAHI,TCAHT)
C ---------------------------------------------------------------------
C - F.Ranjard - 881213
C! fill bank CAHT using bank CAHI
C  CAHT stands for a detector HiT bank known by its name-index NCAHT
C  CAHI stands for a detector HIstory bank known by its name TCAHI
C
C - Input  : NCAHI / I  = name-index of the history bank
C            TCAHT / A  = name of the HIT bank
C
C - Output : CAFIHT/ I  = index of name-bank TCAHT
C                         if 0 means not enough space
C   a work bank CKEY with index JDCKEY is created, filled and dropped.
C   if CAHT bank exists when entering the routine it is dropped.
C ========================================================
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      CHARACTER*4 TCAHT, CHAINT
      PARAMETER (JHISPT=1, JHISAD=2, JHISDE=3)
      PARAMETER (JHITAD=1, JHITDE=2)
      COMMON /CKEYCO/ JDCKEY
      DATA IFI /0/
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
C --------------------------------------------------------------------
C
      IF (IFI.EQ.0) THEN
         JDCKEY = 0
         IFI = 1
      ENDIF
C
      CAFIHT = -1
      JCAHI = IW(NCAHI)
      IF (JCAHI.EQ.0) RETURN
      NRHI = LROWS(JCAHI)
      IF (NRHI.EQ.0) RETURN
      LHIT = JHITDE + LCOLS(JCAHI)-JHISDE
C
C - Drop and then Create CAHT with NRHI rows and LHIT columns
C
      CALL BDROP (IW,TCAHT)
      CALL AUBOS (TCAHT,0,NRHI*LHIT+LMHLEN,JCAHT,IGARB)
      IF (JCAHT.EQ.0) GOTO 998
      IF (IGARB.EQ.1) JCAHI = IW(NCAHI)
      IW(JCAHT+LMHCOL) = LHIT
C
C - Sort CAHI in increasing order of tower address
C
      CALL WBANK (IW,JDCKEY,NRHI,*998)
      IW(JDCKEY-3) = INTCHA('CKEY')
      DO 1 I =1,NRHI
         IW(JDCKEY+I) = KROW(JCAHI,I) + JHISAD
 1    CONTINUE
      CALL SORTZV (IW(1),IW(JDCKEY+1),NRHI,-1,0,1)
C
C - Fill the bank, sum the energy in identical storey
C
      LASAD = -1
      DO 100 I=1,NRHI
         KCAHI = IW(JDCKEY+I) - JHISAD
         IF (IW(KCAHI+JHISAD) .NE. LASAD) THEN
            LASAD = IW(KCAHI+JHISAD)
            KCAHT = KNEXT(JCAHT)
            IW(KCAHT+JHITAD) = IW(KCAHI+JHISAD)
            K = 0
            DO 10 J = JHISDE,LCOLS(JCAHI)
               IW(KCAHT+JHITDE+K) = IW(KCAHI+J)
  10        K = K + 1
            IW(JCAHT+LMHROW) = LROWS(JCAHT) + 1
         ELSE
            K = 0
            DO 20 J = JHISDE,LCOLS(JCAHI)
               IW(KCAHT+JHITDE+K) = IW(KCAHT+JHITDE+K) + IW(KCAHI+J)
  20        K = K + 1
         ENDIF
  100 CONTINUE
C
C - Reduce length of CAHT and drop work bank
      CAFIHT = JCAHT
      CALL AUBPRS (TCAHT)
      CALL WDROP (IW,JDCKEY)
      RETURN
C
C - not enough space
 998  CAFIHT = 0
      END
