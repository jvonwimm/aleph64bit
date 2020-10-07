      SUBROUTINE ECPHCO(IPHCO,IGOO)
C-----------------------------------------------------------------------
CKEY EDIR DIGITAL PATTERN
C! Look id digital pattern linkes to PHCO object.
C-
C   Input  IPHCO : Index of actual PHCO
C   Output IGOO  : = 1 if at least 1 digital pattern linked
C-
C   Called by   : ESWEH
C   Calls  : MAKLIS
C   Input banks : PPOB,PCRL
C-
C                                    Author: M.N.Minard  - 910400
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
      PARAMETER(JPCRPC=1,JPCRPE=2,JPCRPF=3,JPCRPH=4,JPCRPP=5,LPCRLA=5)
C --
      PARAMETER(LENMAX=1000)
      DIMENSION IPLIS(LENMAX)
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
      IGOO = 0
      IPPOB=NLINK('PPOB',0)
      IF(IPPOB.EQ.0) GO TO 999
      IPCRL=NLINK('PCRL',0)
      IF(IPCRL.EQ.0) GO TO 999
      CALL MAKLIS(IPCRL,JPCRPH,JPCRPP,IPHCO,NMATCH,IPLIS,IER)
      IF(IER.NE.0) THEN
         IF(IER.GT.0) THEN
           IF(IW(6).GT.0) WRITE(IW(6),*) 'ECPHCO_error in MAKLIS ',IER
           GO TO 999
         ENDIF
      ENDIF
      IF(NMATCH.GT.0) IGOO = 1
C --
 999  RETURN
      END
