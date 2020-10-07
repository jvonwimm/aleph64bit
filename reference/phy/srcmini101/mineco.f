      SUBROUTINE MINECO
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Ecal calorimeter bank DECO for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : PECO bank
C     Output : DECO bank
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
      PARAMETER(JPECER=1,JPECE1=2,JPECE2=3,JPECTH=4,JPECPH=5,JPECEC=6,
     +          JPECKD=7,JPECCC=8,JPECRB=9,JPECPC=10,LPECOA=10)
      PARAMETER(JDECE0=1,JDECTH=2,JDECPH=3,JDECEF=4,JDECCC=6,LDECOA=6)
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
C++   Pick up PECO bank.
C
      KPECO = NLINK('PECO',1)
      IF (KPECO.LE.0) KPECO = IW(NAMIND('PECO'))
      IF (KPECO.LE.0) RETURN
      NPECO = LROWS(KPECO)
      IF (NPECO.LE.0) RETURN
      NR = IW(KPECO-2)
C
C++   Create DECO bank.
C
      NDECO = NPECO
      LEN = LMHLEN + LDECOA * NDECO
      CALL AUBOS('DECO',NR,LEN, KDECO,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINECO: Cannot create DECO bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KPECO = NLINK('PECO',NR)
      ENDIF
      IW(KDECO+LMHCOL) = LDECOA
      IW(KDECO+LMHROW) = NDECO
C
C++   Loop over PECO storing information in DECO.
C
      DO 100 I=1,NPECO
         IW(KROW(KDECO,I)+JDECE0) = NINT(EFACTM * RTABL(KPECO,I,JPECEC))
         IW(KROW(KDECO,I)+JDECTH) = NINT(AFACTM * RTABL(KPECO,I,JPECTH))
         IW(KROW(KDECO,I)+JDECPH) = NINT(AFACTM * RTABL(KPECO,I,JPECPH))
         IW(KROW(KDECO,I)+JDECEF+0) = NINT(1000.* RTABL(KPECO,I,JPECE1))
         IW(KROW(KDECO,I)+JDECEF+1) = NINT(1000.* RTABL(KPECO,I,JPECE2))
         IW(KROW(KDECO,I)+JDECCC) = ITABL(KPECO,I,JPECCC)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DECO')
C
      RETURN
      END
