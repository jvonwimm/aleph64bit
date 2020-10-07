      SUBROUTINE UTWOCL(NAMI,NUM,ICL1,ICL2,NAMO,IGARB,IER)
C ----------------------------------------------------------------------
C - J.F. Grivaz 15-03-1989
C! Extract and clean up two columns from a Tabular Bank
C - Designed for relation bank, and works correctly only if
C -    all columns are normally filled with positive integers
C - Duplicate rows and rows with at least one null attribute
C -    are removed from the output bank
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
C - Calls UTWOEX, UCLEAN, UBPACK                          from this .HLB
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
      CALL UTWOEX(NAMI,NUM,ICL1,ICL2,NAMO,IGARB,IER)
      IF(IGARB.EQ.2.OR.IER.NE.0) GO TO 100
C
      CALL UCLEAN(NAMO,NUM,IER)
      IF(IER.NE.0) GO TO 100
C
      KTAG = 1
      LTAG = 0
      JTAG = 0
      CALL UBPACK(NAMO,NUM,KTAG,LTAG,JTAG,IER)
      IF(IER.NE.0) GO TO 100
C
      KTAG = 2
      LTAG = 0
      JTAG = 0
      CALL UBPACK(NAMO,NUM,KTAG,LTAG,JTAG,IER)
      IF(IER.NE.0) GO TO 100
C
  100 CONTINUE
      RETURN
      END
