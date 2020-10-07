      SUBROUTINE MINBLM
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Build Mini banks for Monte-Carlo.
C
C     Author: Stephen Haywood      06-Jan-93
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
C++   Fill reduced FKIN bank.
C
      IF (NLINK('DTMC',0).GT.0) THEN
         CALL MINLIS('DTMC')
      ELSE
         CALL MINTMC
      ENDIF
C
C++   Fill reduced FVER bank.
C
      IF (NLINK('DVMC',0).GT.0) THEN
         CALL MINLIS('DVMC')
      ELSE
         CALL MINVMC
      ENDIF
C
C++   Fill reduced FZFR bank.
C
      IF (NLINK('DFMC',0).GT.0) THEN
         CALL MINLIS('DFMC')
      ELSE
         CALL MINFMC
      ENDIF
C
C++   Add further useful banks.
C
      CALL MINLIS('KEVH')
      CALL MINLIS('ASEV')
      IF (LROWS(NLINK('FPOL',0)).GT.0) CALL MINLIS('FPOL')
      IF (LROWS(NLINK('PASL',0)).GT.0) CALL MINLIS('PASL')
      IF (LROWS(NLINK('PITM',0)).GT.0) CALL MINLIS('PITM')
      IF (LROWS(NLINK('FPOI',0)).GT.0) CALL MINLIS('FPOI')
      IF (LROWS(NLINK('PEMH',0)).GT.0) CALL MINLIS('PEMH')
      IF (LROWS(NLINK('PHMH',0)).GT.0) CALL MINLIS('PHMH')
C
C++   Add old matching banks, if new ones don't exist.
C
      IF (NLINK('PASL',0).LE.0 .OR. NLINK('PITM',0).LE.0) THEN
         IF (IW(NAMIND('IPJT')).GT.0) CALL MINLIS('IPJT')
         IF (IW(NAMIND('IRJT')).GT.0) CALL MINLIS('IRJT')
         IF (LROWS(NLINK('PTMA',0)).GT.0) CALL MINLIS('PTMA')
         IF (LROWS(NLINK('PTML',0)).GT.0) CALL MINLIS('PTML')
      ENDIF
C
      RETURN
      END
