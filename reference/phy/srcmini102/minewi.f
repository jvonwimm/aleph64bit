      SUBROUTINE MINEWI
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Ecal wire energy bank DEWI for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : PEWI bank
C     Output : DEWI bank
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
      PARAMETER(JPEWMN=1,JPEWPD=2,JPEWSS=47,JPEWTI=55,LPEWIA=55)
      PARAMETER(JDEWMN=1,JDEWE0=2,JDEWEF=3,LDEWIA=4)
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
C++   Pick up PEWI bank.
C
      KPEWI = NLINK('PEWI',0)
      IF(KPEWI.EQ.0) KPEWI = NLINK('PWEI',0)
      IF(KPEWI.LE.0) RETURN
      NPEWI = LROWS(KPEWI)
      IF(NPEWI.LE.0) RETURN
C
C++   Create DEWI bank.
C
      NDEWI = NPEWI
      LEN = LMHLEN + LDEWIA * NDEWI
      CALL AUBOS('DEWI',0,LEN, KDEWI,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINEWI: Cannot create DEWI bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KPEWI = NLINK('PEWI',0)
      ENDIF
      IW(KDEWI+LMHCOL) = LDEWIA
      IW(KDEWI+LMHROW) = NDEWI
C
C++   Loop over PEWI, sum energies on wires and storing totals in DEWI.
C
      DO 100 I=1,NPEWI
         IW(KROW(KDEWI,I)+JDEWMN) = ITABL(KPEWI,I,JPEWMN)
         EKEV1 = 0.
         EKEV2 = 0.
         EKEV3 = 0.
         DO 110 IPLAN=1,10
  110    EKEV1 = EKEV1 + FLOAT(ITABL(KPEWI,I,JPEWPD-1+IPLAN))
         DO 120 IPLAN=11,33
  120    EKEV2 = EKEV2 + FLOAT(ITABL(KPEWI,I,JPEWPD-1+IPLAN))
         DO 130 IPLAN=34,45
  130    EKEV3 = EKEV3 + FLOAT(ITABL(KPEWI,I,JPEWPD-1+IPLAN))
         EKEV = EKEV1 + EKEV2 + EKEV3
         EGEV = EKEV / 1000000.
         IW(KROW(KDEWI,I)+JDEWE0) = NINT(EFACTM * EGEV)
         IW(KROW(KDEWI,I)+JDEWEF+0) = NINT(1000. * EKEV1/EKEV )
         IW(KROW(KDEWI,I)+JDEWEF+1) = NINT(1000. * EKEV2/EKEV )
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DEWI')
C
      RETURN
      END
