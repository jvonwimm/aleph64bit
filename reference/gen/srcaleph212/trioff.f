      SUBROUTINE TRIOFF (IGOO)
C-----------------------------------------------------------------------
CKEY EDIR TRIGGER QQBAR
C! Trigger off-line for QQbar selection.
C! In absence of ADC information apply offline trigger from the wires.
C-
C   Input  : None
C   Output : IGOO  = 1 satisfy off-line trigger conditions
C                    1.5 Gev in each of the Endcap module
C                 or 6.  Gev in the Barrel
C-
C   Called by   : SELCAL
C   Calls  : None
C   Input banks : PEWI
C-
C                                     Author: M.N.Minard - 910400
C-----------------------------------------------------------------------
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
      DIMENSION  WSUM (2,3)
      EXTERNAL NAMIND
      LOGICAL BTEST
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
      CALL VZERO(WSUM,6)
C --
C   Look for PEWI bank
C --
      NAPEWI = NAMIND ('PEWI')
      KPEWI = IW(NAPEWI)
      IF ( KPEWI.EQ.0) KPEWI = IW(NAMIND('PWEI'))
      NPEWI = 0
      IF ( KPEWI.NE.0) NPEWI= LROWS(KPEWI)
C --
C   Calculate odd & even wires of each module
C --
      DO 10 IPEWI = 1,NPEWI
        JPEWI = KROW ( KPEWI,IPEWI)
        IMOD = IW (JPEWI+1)
        IF ( IMOD.LT.1.OR.IMOD.GT.36) GO TO 10
        ISC = (IMOD-1)/12 +1
        DO 20 IPLAN = 1, 45
          IL = 1
          IF (MOD(IPLAN,2).EQ.0) IL = 2
          EST = FLOAT (IW(JPEWI+1+IPLAN))*.000001
          IF ( EST . LE. 0. ) GO TO 20
          WSUM ( IL,ISC) = WSUM ( IL,ISC) + EST
 20     CONTINUE
 10   CONTINUE
C --
C     Now check trigger
C --
      IGOO = 0
      IF ( WSUM (1,2).GT.3.AND.WSUM(2,2).GT.3) IGOO = 1
      IF ( WSUM (1,1).GT.0.75.AND.WSUM(2,1).GT.0.75) THEN
        IF ( WSUM (1,3).GT.0.75.AND.WSUM(2,3).GT.0.75) IGOO=1
      ENDIF
      RETURN
      END
