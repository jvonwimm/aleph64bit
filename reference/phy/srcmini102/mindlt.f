      SUBROUTINE MINDLT
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Reconstructed tagged lepton bank DDLT for Mini-DST.
C
C     Author: Agnieszka Jacholkowska    24-Oct-94
C
C     Input  : PDLT bank
C     Output : DDLT bank
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
C
      PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
      PARAMETER(JPDLPA=1,JPDLJT=2,JPDLPI=3,JPDLPE=4,JPDLVJ=5,
     &          JPDLFR=6,LPDLTA=6)
      PARAMETER(JDDLPA=1,JDDLJT=2,JDDLPI=3,JDDLPE=4,JDDLVJ=5,
     &          JDDLFR=6,LDDLTA=6)

C!    set of intrinsic functions to handle BOS banks
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
C
C++   Pick up PDLT bank.
C
      KPDLT = NLINK('PDLT',0)
      IF (KPDLT.LE.0) RETURN
      NPDLT = LROWS(KPDLT)
      IF (NPDLT.LE.0) RETURN
C
C++   Create the DDLT bank.
C
      NDDLT = NPDLT
      LEN = LMHLEN + LDDLTA * NDDLT
      CALL AUBOS('DDLT',0,LEN, KDDLT,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINDLT: Cannot create DDLT bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KPDLT = NLINK('PDLT',0)
      ENDIF
      IW(KDDLT+LMHCOL) = LDDLTA
      IW(KDDLT+LMHROW) = NDDLT
c     WRITE(IW(6),'('' MINDLT: DDLT bank lifted'')')
c     PRINT *, LDDLTA, NPDLT, NDDLT, LMHROW, IW(KDDLT+LMHROW)
C
C++   Loop over PDLT storing information in DDLT.
C
      DO 100 I=1,NDDLT
         IW(KROW(KDDLT,I)+JDDLPA) = ITABL(KPDLT,I,JPDLPA)
         IW(KROW(KDDLT,I)+JDDLJT) = ITABL(KPDLT,I,JPDLJT)
         IW(KROW(KDDLT,I)+JDDLPI) = NINT(AFACTM * RTABL(KPDLT,I,JPDLPI))
         IW(KROW(KDDLT,I)+JDDLPE) = NINT(AFACTM * RTABL(KPDLT,I,JPDLPE))
         IW(KROW(KDDLT,I)+JDDLVJ) = ITABL(KPDLT,I,JPDLVJ)

         IW(KROW(KDDLT,I)+JDDLFR) = ITABL(KPDLT,I,JPDLFR)
  100 CONTINUE
c     PRINT *, LDDLTA, NPDLT, NDDLT, LMHROW, IW(KDDLT+LMHROW)
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DDLT')
C
      RETURN
      END
