      SUBROUTINE MINPHC
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PHCO bank from DHCO.
C
C     Author: Stephen Haywood      03-Apr-90
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
      PARAMETER(JDHCE0=1,JDHCTH=2,JDHCPH=3,LDHCOA=3)
      PARAMETER(JPHCER=1,JPHCTH=2,JPHCPH=3,JPHCEC=4,JPHCKD=5,JPHCCC=6,
     +          JPHCRB=7,JPHCNF=8,JPHCPC=9,LPHCOA=9)
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
      KDHCO = NLINK('DHCO',0)
      IF(KDHCO.LE.0) RETURN
C
C++   Create PHCO bank.
C
      NPHCO = LROWS(KDHCO)
      IF(NPHCO.LE.0) RETURN
      LEN = LMHLEN + LPHCOA * NPHCO
      CALL AUBOS('PHCO',0,LEN, KPHCO,IGARB)
      CALL BLIST(IW,'S+','PHCO')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDHCO = NLINK('DHCO',0)
      ENDIF
      IW(KPHCO+LMHCOL) = LPHCOA
      IW(KPHCO+LMHROW) = NPHCO
C
C++   Fill PHCO bank.
C
      DO 100 I=1,NPHCO
         RW(KROW(KPHCO,I)+JPHCEC) = FLOAT(ITABL(KDHCO,I,JDHCE0))/EFACTM
         RW(KROW(KPHCO,I)+JPHCTH) = FLOAT(ITABL(KDHCO,I,JDHCTH))/AFACTM
         RW(KROW(KPHCO,I)+JPHCPH) = FLOAT(ITABL(KDHCO,I,JDHCPH))/AFACTM
  100 CONTINUE
C
      RETURN
      END
