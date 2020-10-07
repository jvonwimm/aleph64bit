      SUBROUTINE MINFZF
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill FZFR from DFMC.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : DFMC bank
C     Output : FZFR bank
C
C     Called by MINFIL
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
C++   Pick up DFMC bank.
C
      KDFMC = NLINK('DFMC',0)
      IF (KDFMC.LE.0) RETURN
      NDFMC = LROWS(KDFMC)
      IF(NDFMC.LE.0) RETURN
C
C++   Create the FZFR bank.
C
      LEN = LMHLEN + LFZFRA * NDFMC
      CALL AUBOS('FZFR',0,LEN, KFZFR,IGARB)
      CALL BLIST(IW,'S+','FZFR')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDFMC = NLINK('DFMC',0)
      ENDIF
      IW(KFZFR+LMHCOL) = LFZFRA
      IW(KFZFR+LMHROW) = NDFMC
C
C++   Fill FZFR bank.
C
      DO I=1,NDFMC
         RW(KROW(KFZFR,I)+JFZFZF) = FLOAT(ITABL(KDFMC,I,JDFMZF))/1000.
      ENDDO
C
      RETURN
      END
