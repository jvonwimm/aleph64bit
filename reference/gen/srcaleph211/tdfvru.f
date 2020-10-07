      INTEGER FUNCTION TDFVRU (IRUN,IROW)
C --------------------------------------------------------------
C!  get a row of a bank
C - F.Ranjard - 900517
CKEY TPC  GET BANK DA
C
C - Input:
C             IRUN   / INTE  = run number
C
C - Output:   TDFVRU / INTE  = TDFV BOS index
C                              =0 means not enough space
C                              <0 means a garbage collection occurded
C             IROW   / INTE  = row # for the known IRUN
C                              0 means IRUN not found
C
C ----------------------------------------------------------------
      SAVE
      CHARACTER*2 DIR
      INTEGER GTDBAS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JTDFFR=1,JTDFLR=2,JTDFVA=3,JTDFVB=6,LTDFVA=8)
      DATA NTDFV /0/ , IPRUN/0/
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
      IF (NTDFV.EQ.0) THEN
         NTDFV = NAMIND('TDFV')
         NCDAF = NAMIND('CDAF')
         LDBAS = JUNIDB(0)
         JTDFV = IW(NTDFV)
         IF (JTDFV.NE.0) THEN
C      NRUN is the bank number of the current TDFV bank
            NRUN = IW(JTDFV-2)
C      LAST is the highest element in the  TDFV,NR=NRUN bank
            LAST = ITABL(JTDFV,LROWS(JTDFV),JTDFLR)
         ENDIF
      ENDIF
C
C - get TDFV bank
C
      IGARB = 0
      NR = IRUN
      DIR = 'LE'
      JTDFV = IW(NTDFV)
 40   IF (JTDFV.EQ.0) THEN
C     get TDFV from the data base if any
C
C     1st  check validity range of the data base for real data
         IF (IW(NCDAF).EQ.0.AND.IPRUN.NE.IRUN.AND.IRUN.GE.2001) THEN
            IPRUN = IRUN
            IGET = GTDBAS (LDBAS,IRUN)
            IF (IGET.NE.0) THEN
               TDFVRU = 0
               RETURN
            ENDIF
         ENDIF
C
         NRUN = NDANR (LDBAS,'TDFV',DIR,NR)
         IF (NRUN.NE.0) THEN
            JTDFV = MDARD (IW,LDBAS,'TDFV',NRUN)
            IF (JTDFV.EQ.0) THEN
               IGARB=1
               CALL BGARB(IW)
               JTDFV = MDARD (IW,LDBAS,'TDFV',NRUN)
               IF (JTDFV.EQ.0) GOTO 60
            ENDIF
C           LAST is the highest element in the d.b NAME,NR=NRUN bank
            LAST = ITABL(JTDFV,LROWS(JTDFV),JTDFLR)
         ENDIF
      ENDIF
C
C - get the row # IROW  which contains the run # NR
C
      IF (JTDFV.GT.0) THEN
 50      LC = LCOLS(JTDFV)
         LR = LROWS(JTDFV)
C
C     IF the run # IRUN is greater than the last run THEN
C        IF a TDFV bank with a higher bank # exists THEN
C           use this TDFV bank
C        ELSE
C           look at the data base with a IRUN greater than the LAST one
C        ENDIF
C     ELSEIF IRUN is smaller than the 1st one THEN
C        look at the data base
C     ELSE
C        find the right row # in TDFV bank
C     ENDIF
C
         IF (IRUN .GT. ITABL(JTDFV,LR,JTDFLR)) THEN
            IF (IW(JTDFV-1) .GT. 0) THEN
               JTDFV = IW(JTDFV-1)
               GOTO 50
            ELSE
               NR = LAST+1
               DIR = 'GE'
               JTDFV = 0
               GOTO 40
            ENDIF
         ELSEIF (IRUN .LT. NRUN) THEN
            DIR = 'LE'
            JTDFV = 0
            GOTO 40
         ELSE
C
            IROW = LOCTAB (IW(JTDFV+LMHLEN+1),LC,LR,JTDFFR,IRUN)
            IF (IROW.EQ.0) THEN
C           IRUN is outside run range
               JTDFV = 0
            ELSEIF (IROW.LT.0) THEN
C           IRUN is between 1st run of row # IROW and 1st run of
C           row # IROW+1
C           check that it is in the run range of row # IROW
               IROW = -IROW
               IF (IRUN.GT.ITABL(JTDFV,IROW,JTDFLR)) JTDFV = 0
            ENDIF
         ENDIF
      ENDIF
C
C - end
C
 60   CONTINUE
      TDFVRU = JTDFV
      IF (IGARB.EQ.1) TDFVRU = -JTDFV
      END