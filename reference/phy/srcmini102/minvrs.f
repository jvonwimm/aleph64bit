      SUBROUTINE MINVRS
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill version information bank DVRS for Mini-DST.
C
C     Author: Stephen Haywood      01-Mar-93
C
C     Input  : from MINVSN and YV0V bank
C     Output : DVRS bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
C
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JDVRMV=1,JDVRV0=2,LDVRSA=2)
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
C++   Create the DVRS bank.
C
      LEN = LMHLEN + LDVRSA
      CALL AUBOS('DVRS',0,LEN, KDVRS,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINVRS: Cannot create DVRS bank'')')
         RETURN
      ENDIF
      IW(KDVRS+LMHCOL) = LDVRSA
      IW(KDVRS+LMHROW) = 1
C
C++   Save Mini version and bank number of YV0V.
C
      IW(KROW(KDVRS,1)+JDVRMV) = MINVSN(DUM)
      KYV0V = IW(NAMIND('YV0V'))
      IF (KYV0V.GT.0) THEN
         NR = IW(KYV0V-2)
      ELSE
         NR = -1
      ENDIF
      IW(KROW(KDVRS,1)+JDVRV0) = NR
C
C++   Add bank to the Mini list.
C
      CALL MINLIS('DVRS')
C
      RETURN
      END
