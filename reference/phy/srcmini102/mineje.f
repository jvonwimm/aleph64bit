      SUBROUTINE MINEJE
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill EJET bank from DJET.
C
C     Author: Stephen Haywood      05-Feb-91
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
      PARAMETER(JDJEPX=1,JDJEPY=2,JDJEPZ=3,JDJEE0=4,LDJETA=4)
      PARAMETER(JEJEPX=1,JEJEPY=2,JEJEPZ=3,JEJEPE=4,LEJETA=4)
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
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
C++   Loop over different energy flow algorithms.
C
      DO 1000 IBNK=0,3
C
      KDJET = NLINK('DJET',IBNK)
      IF(KDJET.LE.0) GOTO 1000
C
C++   Create EJET bank.
C
      NEJET = LROWS(KDJET)
      IF(NEJET.LE.0) GOTO 1000
      LEN = LMHLEN + LEJETA * NEJET
      CALL AUBOS('EJET',IBNK,LEN, KEJET,IGARB)
      CALL BLIST(IW,'S+','EJET')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDJET = NLINK('DJET',IBNK)
      ENDIF
      IW(KEJET+LMHCOL) = LEJETA
      IW(KEJET+LMHROW) = NEJET
C
C++   Fill EJET bank.
C
      DO 100 I=1,NEJET
         RW(KROW(KEJET,I)+JEJEPX) = FLOAT(ITABL(KDJET,I,JDJEPX))/EFACTM
         RW(KROW(KEJET,I)+JEJEPY) = FLOAT(ITABL(KDJET,I,JDJEPY))/EFACTM
         RW(KROW(KEJET,I)+JEJEPZ) = FLOAT(ITABL(KDJET,I,JDJEPZ))/EFACTM
         RW(KROW(KEJET,I)+JEJEPE) = FLOAT(ITABL(KDJET,I,JDJEE0))/EFACTM
  100 CONTINUE
C
 1000 CONTINUE
C
      RETURN
      END
