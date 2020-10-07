      SUBROUTINE MINXTR
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill XTRB bank from DTBP.
C
C     Author: Stephen Haywood      18-Apr-91
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
      PARAMETER(JDTBT1=1,JDTBT2=2,JDTBL2=3,LDTBPA=3)
      PARAMETER(JXTRRP=1,JXTRRN=4,LXTRBA=4)
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
C++   Pick up links.
C
      KDTBP = NLINK('DTBP',100)
      IF (KDTBP.LE.0) THEN
         CALL MINUPD('DTBP')
         KDTBP = NLINK('DTBP',100)
         IF (KDTBP.LE.0) RETURN
      ENDIF
C
C++   Create XTRB bank.
C
      LEN = LMHLEN + LXTRBA
      CALL AUBOS('XTRB',0,LEN, KXTRB,IGARB)
      CALL BLIST(IW,'S+','XTRB')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTBP = NLINK('DTBP',100)
      ENDIF
      IW(KXTRB+LMHCOL) = LXTRBA
      IW(KXTRB+LMHROW) = 1
C
      IW(KROW(KXTRB,1)+JXTRRP+0) = ITABL(KDTBP,1,JDTBT1)
      IW(KROW(KXTRB,1)+JXTRRP+1) = ITABL(KDTBP,1,JDTBT2)
      IW(KROW(KXTRB,1)+JXTRRP+2) = ITABL(KDTBP,1,JDTBL2)
      IW(KROW(KXTRB,1)+JXTRRP+3) = INTCHA('TRB1')
C
      RETURN
      END
