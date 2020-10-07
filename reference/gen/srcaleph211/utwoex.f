      SUBROUTINE UTWOEX(NAMI,NUM,ICL1,ICL2,NAMO,IGARB,IER)
C ----------------------------------------------------------------------
C - J.F. Grivaz 15-03-1989
C! Extract two columns from a Tabular Bank
C - Input :
C -   NAMI  = Name of the bank from which the extraction is to be done
C -   NUM = Number of the bank from which the extraction is to be done
C -   ICL1  = First column to extract (will be column 1 in output bank)
C -   ICL2 = Second column to extract (will be column 2 in output bank)
C -   NAMO = Name of the output bank to be created (its number = NUM)
C - Output
C -   IGARB = 1 if Garbage Collection occured
C -           2 if not enough space after Garbage Collection
C -   IER = 1 if Unsuccessful
C - Calls AUBOS                                           from ALEPHLIB
C  ----------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      CHARACTER*4 NAMI,NAMO
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
      KINDX = NLINK(NAMI,NUM)
      IF(KINDX.LE.0) GO TO 999
      NCOLS = LCOLS(KINDX)
      IF (ICL1.LE.0.OR.ICL1.GT.NCOLS) GO TO 999
      IF (ICL2.LE.0.OR.ICL2.GT.NCOLS) GO TO 999
      NROWS = LROWS(KINDX)
C
      LTOTO = NROWS*2 + LMHLEN
      CALL AUBOS(NAMO,NUM,LTOTO,KONDX,IGARB)
      IF(IGARB.GE.2) GO TO 100
      IF(IGARB.EQ.1) THEN
        KINDX = NLINK(NAMI,NUM)
        IF(KINDX.LE.0) GO TO 999
      ENDIF
C
      JINDX = KINDX + LMHLEN
      JONDX = KONDX + LMHLEN
      DO 1 I = 1,NROWS
        IW(JONDX+1) = IW(JINDX+ICL1)
        IW(JONDX+2) = IW(JINDX+ICL2)
        JINDX = JINDX + NCOLS
        JONDX = JONDX + 2
    1 CONTINUE
      IW(KONDX+1) = 2
      IW(KONDX+2) = NROWS
      GO TO 100
C
  999 CONTINUE
      IER = 1
C
  100 CONTINUE
      RETURN
      END
