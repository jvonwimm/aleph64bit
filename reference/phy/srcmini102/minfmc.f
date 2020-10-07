      SUBROUTINE MINFMC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill fragmentation MC bank DFMC for Mini-DST.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : FZFR bank
C     Output : DFMC bank
C
C     Called by MINDST
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
      PARAMETER(JFZFZF=1,LFZFRA=1)
      PARAMETER(JDFMZF=1,LDFMCA=1)
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
C++   Pick up FZFR bank.
C
      KFZFR = NLINK('FZFR',0)
      IF (KFZFR.LE.0) RETURN
      NFZFR = LROWS(KFZFR)
      IF(NFZFR.LE.0) RETURN
C
C++   Create the DFMC bank.
C
      LEN = LMHLEN + LDFMCA * NFZFR
      CALL AUBOS('DFMC',0,LEN, KDFMC,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINFMC: Cannot create DFMC bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KFZFR = NLINK('FZFR',0)
      ENDIF
      IW(KDFMC+LMHCOL) = LDFMCA
      IW(KDFMC+LMHROW) = NFZFR
C
C++   Copy FZFR to DFMC, integerising fragmention fractions.
C
      DO I=1,NFZFR
         IW(KROW(KDFMC,I)+JDFMZF) = NINT(1000 * RTABL(KFZFR,I,JFZFZF))
      ENDDO
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DFMC')
C
      RETURN
      END
