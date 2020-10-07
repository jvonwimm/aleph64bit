      INTEGER FUNCTION ALGTBK (NAME,NR,NEW,JCOL,MXROW,LNAMEA,IROW)
C --------------------------------------------------------------
C! create/ get a bank
C - F.Ranjard - 900517
CKEY ALEF GET BANK DA
C
C - Input:    NAME   / A4    = bank name
C             NR     / INTE  = bank number
C             NEW    / INTE  = row address
C             JCOL   / INTE  = col. number to be considered
C             MXROW  / INTE  = max. number of rows
C             LNAMEA / INTE  = number of columns
C
C - Output:   ALGTBK / INTE  = NAME BOS index
C                              =0 means not enough space
C                              <0 means a garbage collection occurded
C             IROW   / INTE  = row # for the known NEW
C                              0 means NEW does not exist
C                              <0 NEW should go in the middle
C
C ----------------------------------------------------------------
      CHARACTER*(*) NAME
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
C ----------------------------------------------------------------
C
C - 1st entry
C
      NNAME = NAMIND(NAME)
      LDBAS = JUNIDB(0)
      IGARB = 0
C
C - get NAME bank
C
      JNAME = IW(NNAME)
      IF (JNAME.EQ.0) THEN
C     get NAME from the data base if any
         NRUN = NDANR (LDBAS,NAME,'LE',NR)
         IF (NRUN.NE.0) THEN
            JNAME = MDARD (IW,LDBAS,NAME,NRUN)
            IF (JNAME.EQ.0) THEN
               IGARB=1
               CALL BGARB(IW)
               JNAME = MDARD (IW,LDBAS,NAME,NRUN)
               IF (JNAME.EQ.0) GOTO 60
            ENDIF
         ENDIF
      ENDIF
C
C - is it a new row or an update
C
      ISORT = 0
      IF (JNAME.GT.0) THEN
         LC = LCOLS(JNAME)
         LR = LROWS(JNAME)
         IROW = LOCTAB (IW(JNAME+LMHLEN+1),LC,LR,JCOL,NEW)
         IF (IROW.GT.0) GOTO 60
         IF (IROW.NE.-LROWS(JNAME))ISORT = 1
      ENDIF
C
C - If more than MXROW rows THEN start a new bank
C
      IF (JNAME.GT.0) THEN
         IF (LROWS(JNAME).GE.MXROW) JNAME = 0
      ENDIF
C
C - create the NAME bank or increase the bank length if necessary
C
      IF (JNAME.GT.0) THEN
         NRUN = IW(JNAME-2)
         IF (LFRROW(JNAME).GE.1) GOTO 50
      ELSE
         NRUN = NR
      ENDIF
      CALL AUBOS (NAME,NRUN,MXROW*LNAMEA+LMHLEN,JNAME,IGARB)
      IF (JNAME.EQ.0) GOTO 60
      IW(JNAME+LMHCOL)=LNAMEA
 50   IROW = LROWS(JNAME)+1
      IW(JNAME+LMHROW) = IROW
C
C - if a sorting is necessary then return IROW=-IROW
C
      IF (ISORT.EQ.1) IROW = -IROW
C
C - end
C
 60   CONTINUE
      ALGTBK = JNAME
      IF (IGARB.EQ.1) ALGTBK = -JNAME
      END
