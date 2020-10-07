      REAL FUNCTION EMTZER ( IMODU )
C -------------------------------------------------------------------
C! return tzero of an ECAL Module # IMODU
C  return -9999. if PEWI/PWEI does exist or if IMODU is not there
CKEY ECAL T0
C - M.N.Minard - 950510
C ------------------------------------------------------------------
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
C ----------------------------------------------------------
      EMTZER = -9999.
      KPEWI = NLINK ( 'PEWI',0)
      IF (KPEWI.EQ.0) KPEWI = NLINK('PWEI',0)
      IF (KPEWI.GT.0) THEN
        LCOL = LCOLS(KPEWI)
        DO IPEWI = 1,LROWS(KPEWI)
          IF ( ITABL(KPEWI,IPEWI,1).EQ.IMODU) THEN
             EMTZER = REAL (ITABL (KPEWI,IPEWI,LCOL))
          ENDIF
        ENDDO
      ENDIF
      END
