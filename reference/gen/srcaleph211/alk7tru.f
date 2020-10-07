      INTEGER FUNCTION ALK7TRU (LURUK7,TYPE,TAPE,JDRUNL)
C --------------------------------------------------------
C - F.Ranjard - 900627
C! get list of runs of a given type residing on a given tape
CKEY ALEF TAPE  RUN / USER
C
C - Input     : LURUK7  / INTE  = open RUK7FILE.UPDATE on unit LURUK7
C               TYPE    / A     = data type
C                                 'RAW'/'POT'/'DST'/'MINI'/'NANO'
C               TAPE    / A     = tape number
C
C - Output    : ALK7FRU   / INTE  = error code
C                                 = 0  OK
C                                 = 1  cannot open RUK7FILE
C                                 = 2  empty file
C                                 = 3  too many runs, increase K7COM
C                                 = 23 wrong data type
C                                 = 24 JDRUNL bank does not exist
C                                 = 25 TAPE does not exist
C
C   IF TAPE is in the list THEN
C      return in JDRUNL work bank the list of runs present on TAPE
C      IW(JDRUNL+1) = INTCHA(TAPE(1:4))
C      IW(JDRUNL+2) = INTCHA(TAPE(5:8))
C      IW(JDRUNL+3) = INTCHA(TAPE(9:12))
C      IW(JDRUNL+4) = number of runs on TAPE
C      IW(JDRUNL+5) = 1st run on TAPE
C      ...
C      IW(JDRUNL+IW(JDRUNL+4)) = last run on TAPE
C   ENDIF
C ---------------------------------------------------------
      CHARACTER*(*) TYPE,TAPE
      CHARACTER*12 TRUN
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      CHARACTER  CHAINT*4
      INTEGER ALK7OP
      PARAMETER (MXRUN=200, NXRUN=50)
C! keep content of RUNCARTS.LIST file
C! keep content of RUNCARTS.LIST file
      PARAMETER(MXLRUN=2500,MXSEG=4,MXTYP=5,LK7=9)
      INTEGER K7LRUN
C - 22500=MXLRUN*LK7 (9 characters per K7 for MXLRUN runs)
      CHARACTER*22500 K7CART
C - 5=MXTYP the number of various data types ('RPDMN')
      CHARACTER*5 K7TYPE
      COMMON/ALK7COM/ K7SEG,K7LINE(MXSEG),K7LRUN(MXLRUN,MXSEG),
     &                K7CART(MXTYP,MXSEG),K7TYPE
      SAVE NK7
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
      IER = 0
C - at 1st entry :
C
       IF (IFIR.EQ.0) THEN
          IFIR = 1
          NK7 = (LK7+3)/4          ! # of words to contain this # of cha
          JDRUNL = 0
          CALL WBANK (IW,JDRUNL,MXRUN+NK7+1,*998)
          DO I=1,NK7
             IW(JDRUNL+I)=INTCHA ('    ')
          ENDDO
          IW(JDRUNL+NK7+1)=0
          IW(JDRUNL-3)=INTCHA ('RUNL')
C
C       open the RUK7FILE.UPDATE file
          IER = ALK7OP (LURUK7)
          IF (IER.NE.0) GOTO 999
       ENDIF
C
C - next entry:
C
 10    CONTINUE
       LT = LNBLNK(TAPE)
C
C    check JDRUNL bank
       IF (JDRUNL .LE.0) THEN
          IER = 24
          CALL ALTELL ('ALK7TRU: no JDRUNL bank progamming error',0,
     &                 'RETURN')
          GOTO 998
       ENDIF
C
C    tape number in JDRUNL
       CALL ALSTIN (IW(JDRUNL+1),NK7,TRUN)
       IF (TRUN(1:4).NE.'    ') THEN
          IF (TRUN(1:LT).EQ.TAPE(1:LT)) GOTO 999
       ENDIF
C
C    check data type
       ITYP = INDEX (K7TYPE,TYPE(1:1))
       IF (ITYP.EQ.0) THEN
          IER = 23
          GOTO 999
       ENDIF
C
C    get run #s on TAPE K7
       LRUN = 0
       DO 30 ISEG=1,K7SEG
          IPOS = 1
          IER = 0
 20       JPOS = INDEX (K7CART(ITYP,ISEG)(IPOS:),TAPE(1:LT))
          IF (JPOS.EQ.0) THEN
             IF (IPOS.EQ.1) THEN
                IF (LRUN.GT.0) GOTO 998
                IER = 25
             ELSE
                CALL ALINST (TAPE,IW(JDRUNL+1),NK7)
                IW(JDRUNL+NK7+1) = LRUN
             ENDIF
             GOTO 30
          ENDIF
          IF (LRUN+NK7+1.GE.IW(JDRUNL)) THEN
             CALL WBANK (IW,JDRUNL,IW(JDRUNL)+NXRUN,*998)
          ENDIF
          JPOS = JPOS+IPOS-1
          LINE = (JPOS-1)/LK7 + 1
          LRUN = LRUN+1
          IW(JDRUNL+NK7+LRUN) = K7LRUN(LINE,ISEG)
          IPOS = JPOS+LK7
          GOTO 20
 30    CONTINUE
C
 998   CONTINUE
       CALL WBANK (IW,JDRUNL,LRUN+NK7,*999)
 999   ALK7TRU = IER
C
       END
