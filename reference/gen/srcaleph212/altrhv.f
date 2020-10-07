      INTEGER FUNCTION ALTRHV (IBITHV)
C --------------------------------------------------------------
CKEY ALEF TRIGGER HV
C - F.Ranjard -940426
C! returns trigger HV word from XTCN or X1RG
C - Output : - IBITHV  / I = word 4
C              ALTRHV  / I = 0 if no bank there
C                            1 if XTCN is there
C                            2 if X1RG is there
C
C ----------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      SAVE NXTCN, NX1RG
      DATA NXTCN / 0/
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
C ------------------------------------------------------------
C - 1st entry : get name indices
      IF (NXTCN.EQ.0) THEN
        NXTCN = NAMIND ('XTCN')
        NX1RG = NAMIND ('X1RG')
      ENDIF
C
C - next entry
      IF (IW(NX1RG).NE.0) THEN
         JTRIG = IW(NX1RG)
         IROW  = 2
         ICOL  = 4
         ALTRHV = 2
      ELSEIF (IW(NXTCN).NE.0) THEN
         JTRIG = IW(NXTCN)
         IROW  = 1
         ICOL  = 7
         ALTRHV = 1
      ELSE
         ALTRHV = 0
         RETURN
      ENDIF
C
C - get HV trigger word
      IBITHV = ITABL (JTRIG,IROW,ICOL)
C
      END
