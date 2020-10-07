      INTEGER FUNCTION LFILRU (IRUN,IROW)
C --------------------------------------------------------------
C!  get a row of a bank
C - F.Ranjard - 900517
CKEY ALEF GET BANK DA
C
C - Input:
C             IRUN   / INTE  = run number
C
C - Output:   LFILRU / INTE  = LFIL BOS index
C                              =0 means not enough space
C                              <0 means a garbage collection occurded
C             IROW   / INTE  = row # for the known IRUN
C                              0 means IRUN not found
C
C ----------------------------------------------------------------
      CHARACTER*2 DIR
      INTEGER GTDBAS
      LOGICAL GREATER,SMALLER,FDBASE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JLFILF=1,JLFIFR=2,JLFILR=3,JLFINV=4,JLFILE=5,JLFIBX=6,
     +          JLFIBY=7,JLFIBZ=8,JLFIEX=9,JLFIEY=10,JLFIEZ=11,
     +          JLFIOF=12,LLFILA=12)
      SAVE NCDAF, LDBAS, IPRUN, NRUN, LAST
      DATA NLFIL /0/
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
C ----------------------------------------------------------------
C
C - 1st entry
C
      IF (NLFIL.EQ.0) THEN
         NLFIL = NAMIND('LFIL')
         NCDAF = NAMIND('CDAF')
         LDBAS = JUNIDB(0)
         JCOL  = JLFIFR
         IPRUN = -1
         JLFIL = IW(NLFIL)
         IF (JLFIL.NE.0) THEN
C      NRUN is the bank number of the current LFIL bank
            NRUN = IW(JLFIL-2)
C      LAST is the highest element in the  LFIL,NR=NRUN bank
            LAST = ITABL(JLFIL,LROWS(JLFIL),JLFILR)
         ENDIF
      ENDIF
C
C - get LFIL bank
C
      IGARB = 0
      NR = IRUN
      DIR = 'LE'
      GREATER = .FALSE.
      SMALLER = .FALSE.
      FDBASE  = .FALSE.
      JLFIL = IW(NLFIL)
 40   IF (JLFIL.EQ.0) THEN
C     get LFIL from the data base if any
C
C     1st  check validity range of the data base for real data
         IF (IW(NCDAF).EQ.0.AND.IPRUN.NE.IRUN.AND.IRUN.GE.2001) THEN
            IPRUN = IRUN
            IGET = GTDBAS (LDBAS,IRUN)
            IF (IGET.NE.0) THEN
               LFILRU = 0
               RETURN
            ENDIF
         ENDIF
C
         NRUN = NDANR (LDBAS,'LFIL',DIR,NR)
         IF (NRUN.NE.0) THEN
            JLFIL = MDARD (IW,LDBAS,'LFIL',NRUN)
            IF (JLFIL.EQ.0) THEN
               IGARB=1
               CALL BGARB(IW)
               JLFIL = MDARD (IW,LDBAS,'LFIL',NRUN)
               IF (JLFIL.EQ.0) GOTO 60
            ENDIF
C           LAST is the highest element in the d.b NAME,NR=NRUN bank
            LAST = ITABL(JLFIL,LROWS(JLFIL),JLFILR)
            FDBASE = .TRUE.
         ENDIF
      ENDIF
C
C - get the row # IROW  which contains the run # NR
C
      IF (JLFIL.GT.0) THEN
 50      LC = LCOLS(JLFIL)
         LR = LROWS(JLFIL)
C
C     IF the run # IRUN is greater than the last run THEN
C        IF a LFIL bank with a higher bank # exists THEN
C           use this LFIL bank
C        ELSE
C           look at the data base with a IRUN greater than the LAST one
C        ENDIF
C     ELSEIF IRUN is smaller than the 1st one THEN
C        look at the data base
C     ELSE
C        find the right row # in LFIL bank
C     ENDIF
C
         IF (IRUN .GT. ITABL(JLFIL,LR,JLFILR)) THEN
            IF (SMALLER .AND. FDBASE) GOTO 60
            GREATER = .TRUE.
            IF (IW(JLFIL-1) .GT. 0) THEN
               JLFIL = IW(JLFIL-1)
               GOTO 50
            ELSE
               NR = LAST+1
               DIR = 'GE'
               JLFIL = 0
               GOTO 40
            ENDIF
         ELSEIF (IRUN .LT. ITABL(JLFIL,1,JLFIFR)) THEN
            IF (GREATER .AND. FDBASE) GOTO 60
            SMALLER = .TRUE.
            DIR = 'LE'
            JLFIL = 0
            GOTO 40
         ELSE
C
            IROW = LOCTAB (IW(JLFIL+LMHLEN+1),LC,LR,JLFIFR,IRUN)
            IF (IROW.EQ.0) THEN
C           IRUN is outside run range
               JLFIL = 0
            ELSEIF (IROW.LT.0) THEN
C           IRUN is between 1st run of row # IROW and 1st run of
C           row # IROW+1
C           check that it is in the run range of row # IROW
               IROW = -IROW
               IF (IRUN.GT.ITABL(JLFIL,IROW,JLFILR)) JLFIL = 0
            ENDIF
         ENDIF
      ENDIF
C
C - end
C
 60   CONTINUE
      LFILRU = JLFIL
      IF (IGARB.EQ.1) LFILRU = -JLFIL
      END
