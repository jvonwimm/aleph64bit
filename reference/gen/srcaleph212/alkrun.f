      INTEGER FUNCTION ALKRUN (IDGEN,NOTRK,TITLE)
C -------------------------------------------------------------------
C - F.Ranjard - 870331
C! Build kine run header KRUN
C  call ALSEED to get seeds of random generator number
C - Input  :  IDGEN   = generator identifier
C             NOTRK   = NOtracking marker word
C             TITLE   = run title (up to 48 characters)
C - Output :  ALKRUN  = KRUN bank index
C                       0 means not enough space to book the bank
C  ---------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JKRUGI=1,JKRUNO=2,JKRURT=3,JKRUFS=15,JKRUSS=16,
     +          LKRUNA=16)
      CHARACTER*(*) TITLE
      EXTERNAL INTCHA
      PARAMETER (LINTE = 2, LCHA4 = 12)
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
C --------------------------------------------------------------------
C - Book the bank 'KRUN'
      CALL AUBOS ('KRUN',0,LMHLEN+LKRUNA,JKRUN,IGARB)
      IF (JKRUN.EQ.0) GOTO 999
      IW(JKRUN+LMHCOL) = LKRUNA
      IW(JKRUN+LMHROW) = 1
      CALL BKFMT ('KRUN','4I,12A,2I')
C
C - fill the 1st 14 words of the row
      KKRUN = JKRUN + LMHLEN
      IW(KKRUN+JKRUGI) = IDGEN
      IW(KKRUN+JKRUNO) = NOTRK
      DO 10 I = 1,LCHA4
         IW(KKRUN+JKRURT+I-1) = INTCHA (TITLE(I*4-3:I*4))
 10   CONTINUE
C
C - get the random number seeds and the random number generator type
      CALL ALSEED (IRNDT,ISD1,ISD2)
C   if RANMAR generator then store lab seeds
      IF (IRNDT .EQ. 3) THEN
         IW(KKRUN+JKRUFS) = ISD1
         IW(KKRUN+JKRUSS) = ISD2
      ENDIF
C
 999  CONTINUE
      ALKRUN = JKRUN
      END
