      SUBROUTINE MINHCO
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Hcal calorimeter bank DHCO for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : PHCO bank
C     Output : DHCO bank
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
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
      PARAMETER(JPHCER=1,JPHCTH=2,JPHCPH=3,JPHCEC=4,JPHCKD=5,JPHCCC=6,
     +          JPHCRB=7,JPHCNF=8,JPHCPC=9,LPHCOA=9)
      PARAMETER(JDHCE0=1,JDHCTH=2,JDHCPH=3,LDHCOA=3)
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
C++   Pick up PHCO bank.
C
      KPHCO = NLINK('PHCO',0)
      IF(KPHCO.GT.0) THEN
         NPHCO = LROWS(KPHCO)
      ELSE
         NPHCO = 0
      ENDIF
      IF(NPHCO.LE.0) RETURN
C
C++   Create DHCO bank.
C
      NDHCO = NPHCO
      LEN = LMHLEN + LDHCOA * NDHCO
      CALL AUBOS('DHCO',0,LEN, KDHCO,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINHCO: Cannot create DHCO bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KPHCO = NLINK('PHCO',0)
      ENDIF
      IW(KDHCO+LMHCOL) = LDHCOA
      IW(KDHCO+LMHROW) = NDHCO
C
C++   Loop over PHCO storing information in DHCO.
C
      DO 100 I=1,NPHCO
         IW(KROW(KDHCO,I)+JDHCE0) = NINT(EFACTM * RTABL(KPHCO,I,JPHCEC))
         IW(KROW(KDHCO,I)+JDHCTH) = NINT(AFACTM * RTABL(KPHCO,I,JPHCTH))
         IW(KROW(KDHCO,I)+JDHCPH) = NINT(AFACTM * RTABL(KPHCO,I,JPHCPH))
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DHCO')
C
      RETURN
      END
