      INTEGER FUNCTION ALFMT (LUFMT,NAME,FMT)
C --------------------------------------------------------
C - F.Ranjard - 900627
C! get bank format
CKEY ALEF BOS BANK FORMAT / USER
C
C - Input     : LUFMT   / INTE  = open BANKAL.FMT on unit LUFMT
C               NAME    / A4    = bank name
C                                 if NAME.eq.'ALL ' then create all
C                                 formats at once
C
C - Output    : FMT     / A75  = bank format
C                                 if NAME.eq.'ALL ' then FMT=' '
C               ALFMT   / INTE  = error code
C                                 = 0  OK
C                                 = 1  no format (default I is used)
C - open BANKAL.FMT which contains format of all known banks
C   on unit LUFMT
C   IF NAME.eq.'ALL ' THEN
C      create formats for all banks found in BANKAL.FMT
C   ELSE
C      get the format of NAME bank
C      call BKFMT for this NAME and Format
C   ENDIF
C ---------------------------------------------------------
      SAVE  BFMT , BANK , LC
      CHARACTER TEXT*75
      CHARACTER*(*) FMT
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (NAMAX=1500)
      CHARACTER*4 NAME,CHAINT,NAMO
      CHARACTER*75 BFMT(NAMAX)
      CHARACTER*6000 BANK
      DATA IFIR / 0/
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
C
C ------------------------------------------------------------------
C
C - at 1st entry : open the BANKAL.FMT file
C
       IF (IFIR.EQ.0) THEN
          IFIR = 1
          CALL AGTFIL ('FFMT','READ',LUFMT,IER)
          IF (IER.NE.0) THEN
             CALL AOPEN (LUFMT,'BANKALFMT','CARDS','DISK',IER)
             IF (IER.NE.0) THEN
                IF (IW(6).NE.0) WRITE (IW(6),*)
     &          ' **** ALFMT*** BANKAL.FMT not found - EXIT'
                CALL EXIT
             ENDIF
          ENDIF
          LINE = 0
          BANK = '    '
 10       LINE = LINE+1
          IF (LINE .GT. NAMAX) THEN
             WRITE (IW(6),*) ' ***ALFMT*** too many banks ,skip'
     &        , ' end of file - some  banks will have no format'
             GOTO 20
          ENDIF
          LC = (LINE-1)*4 + 1
          READ (LUFMT,'(A4,1X,A)',END=20) BANK(LC:LC+3),BFMT(LINE)
          IF (NAME(1:3).EQ.'ALL') CALL BKFMT(BANK(LC:LC+3),BFMT(LINE))
          GOTO 10
 20       LINE = LINE-1
          LC = LINE*4
          IF (IW(6).NE.0) WRITE (IW(6),*)
     &                   ' *** ALFMT*** BANKAL.FMT on unit ',LUFMT,
     &                   ' contains ',LINE,' bank names'
       ENDIF
C
C - get the format for NAME bank
C
       FMT = ' '
       IER = 0
       IF (NAME.EQ.'ALL ') THEN
          DO L=1,LINE
             LB = (L-1)*4 + 1
             CALL BOSFMT(BANK(LB:LB+3),TEXT)
             LTEXT = LNBLNK(TEXT)
             LBFMT = LNBLNK(BFMT(L))
             IF (TEXT(1:LTEXT).NE.BFMT(L)(1:LBFMT)) THEN
                CALL BKFMT(BANK(LB:LB+3),BFMT(L))
             ENDIF
          ENDDO
          GOTO 999
       ENDIF
C
       I1 = 1
 30    IPOS = 0
       IF (I1.LT.LC) IPOS = INDEX (BANK(I1:LC),NAME)
       IF (IPOS.EQ.0) THEN
C      default format is used
          FMT = 'I'
          IER = 1
          CALL BKFMT (NAME,FMT)
       ELSEIF (MOD(IPOS,4).NE.1) THEN
          IER = 2
          I1 = (I1-1+IPOS+3)/4
          I1 = I1*4 + 1
          GOTO 30
       ELSE
          IER = 0
          LI = (I1-1+IPOS)/4 + 1
          FMT = BFMT(LI)
          LF = LNBLNK(FMT)
          CALL BKFMT (NAME,FMT(1:LF))
       ENDIF
C
 999   CONTINUE
       ALFMT = IER
C
       END
