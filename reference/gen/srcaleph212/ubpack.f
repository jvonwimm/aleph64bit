      SUBROUTINE UBPACK(NAME,NUM,KTAG,LTAG,JTAG,IER)
C ----------------------------------------------------------------------
C - J.F. Grivaz 15-03-1989
C! Remove marked rows from a Tabular Bank
C - Input :
C -   NAME  = Bank name
C -   NUM   = Bank number
C -   KTAG  = Column where check is to be done. Let IVAL be the content
C -           This column should contain integers
C -   LTAG  = Value to which IVAL is to be compared
C -   JTAG  = Indicates which type of comparison is to be made
C -           -1 means : Kill row if IVAL < LTAG
C -           +1 means : Kill row if IVAL > LTAG
C -            0 means : Kill row if IVAL = LTAG
C - Output :
C -   IER = 1 if Unsuccessful
C  ----------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      CHARACTER*4 NAME
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
C ----------------------------------------------------------------------
C
      KINDX = NLINK(NAME,NUM)
      IF(KINDX.LE.0) GO TO 999
      NCOLS = LCOLS(KINDX)
      IF(KTAG.LE.0.OR.KTAG.GT.NCOLS) GO TO 999
      NROWS = LROWS(KINDX)
      JNEXT = KNEXT(KINDX)
C
      NKILL = 0
      JINDX = JNEXT
      DO 1 IROW = NROWS,1,-1
        JINDX = JINDX - NCOLS
        IVAL = IW(JINDX+KTAG)
        IF(JTAG.LT.0) THEN
          IF(IVAL.GE.LTAG) GO TO 1
        ELSE IF(JTAG.GT.0) THEN
          IF(IVAL.LE.LTAG) GO TO 1
        ELSE
          IF(IVAL.NE.LTAG) GO TO 1
        ENDIF
        I1 = JINDX + NCOLS + 1
        I2 = JNEXT
        DO 2 I = I1,I2
          IW(I-NCOLS) = IW(I)
    2   CONTINUE
        NKILL = NKILL + 1
    1 CONTINUE
C
      IW(KINDX+2) = NROWS - NKILL
      LTOTX = NCOLS*(NROWS-NKILL)+LMHLEN
      KINDX = NBANK(NAME,NUM,LTOTX)
      IF(KINDX.LE.0) GO TO 999
      GO TO 100
C
  999 CONTINUE
      IER = 1
C
  100 CONTINUE
      END
