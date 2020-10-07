      SUBROUTINE ESUMW
C----------------------------------------------------------------------
CKEY EDIR ECAL WIRE CLASS17
C! Calculate Ecal wire energy.
C-
C   Input  : None
C   Output : None
C-
C   Called by   : SELCAL
C   Calls  : None
C   Input banks : PEWI
C-
C                          Authors: M.N. Minard + M. Pepe     15/09/89
C----------------------------------------------------------------------
      SAVE
C --
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C --
      COMMON / EWIR / EWIRE ( 36 )
C --
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
C --
      NAPEWI = NAMIND ('PEWI')
      KPEWI = IW ( NAPEWI)
      IF ( KPEWI.EQ.0) KPEWI = IW(NAMIND('PWEI'))
      CALL VZERO ( EWIRE,36)
      IF ( KPEWI.EQ.0) GO TO 900
      NROW = LROWS ( KPEWI)
      DO 10 IROW = 1,NROW
        JROW = KROW ( KPEWI,IROW)
        IMOD = IW (JROW+1)
        IF ( IMOD.LT.1.OR.IMOD.GT.36) GO TO 10
        DO 20 IK =1,45
           EWIRE( IMOD ) = EWIRE(IMOD) + FLOAT(IW(JROW+1+IK))/1000000.
 20     CONTINUE
 10   CONTINUE
C --
 900     CONTINUE
C --
         RETURN
         END
