      SUBROUTINE UCLEAN(NAME,NUM,IER)
C ----------------------------------------------------------------------
C - J.F. Grivaz 15-03-1989
C! Remove duplicate rows from a Tabular Bank
C - Designed for relation bank, and works correctly only if
C - the first column is normally filled with a positive integer
C - Input :
C -   NAME  = Bank name
C -   NUM   = Bank number
C - Output :
C -   IER = 1 if Unsuccessful
C - Calls UBPACK
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
      NROWS = LROWS(KINDX)
C
      KIND1 = KINDX + LMHLEN
      DO 1 I1 = 1,NROWS-1
        IF(IW(KIND1+1).LT.0) GO TO 2
        KIND2 = KIND1 + NCOLS
        DO 3 I2 = I1+1,NROWS
          IF(IW(KIND2+1).LT.0.) GO TO 4
          DO 5 J = 1,NCOLS
            IF(IW(KIND2+J).NE.IW(KIND1+J)) GO TO 4
    5     CONTINUE
          IW(KIND2+1)=-IW(KIND2+1)
    4     CONTINUE
          KIND2 = KIND2 + NCOLS
    3   CONTINUE
    2   CONTINUE
        KIND1 = KIND1 + NCOLS
    1 CONTINUE
C
      KTAG = 1
      LTAG = 0
      JTAG = -1
      CALL UBPACK(NAME,NUM,KTAG,LTAG,JTAG,IER)
      IF(IER.NE.0) GO TO 100
      GO TO 100
C
  999 CONTINUE
      IER = 1
C
  100 CONTINUE
      RETURN
      END
