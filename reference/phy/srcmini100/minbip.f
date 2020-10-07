      SUBROUTINE MINBIP
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Swap bank DBTG and NBIP.
C
C     Author: Agnieszka Jacholkowska 1-Oct-94
C
C     Input  : DBTG bank
C     Output : NBIP bank
C
C     Called by ALPHA
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      LOGICAL GOTIT
C
C!    set of intrinsic functions to handle BOS banks
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
C
C++   Swap DBTG bank into NBIP bank.
C
      KNBIP =  NSWAP('DBTG', 1,'NBIP',1)
      CALL BLIST(IW,'E+','NBIP')
      IF(KNBIP.LE.0) GOTO 1000
C
C
 1000 CONTINUE
C
      RETURN
      END
