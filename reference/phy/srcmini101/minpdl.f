      SUBROUTINE MINPDL
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill PDLT bank from DDLT.
C
C     Author: Agnieszka Jacholkowska    24-Oct-94
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
C++   Pick up DDLT bank.
C
      KDDLT = NLINK('DDLT',0)
      IF (KDDLT.LE.0) RETURN
      NDDLT = LROWS(KDDLT)
      IF (NDDLT.LE.0) RETURN
C
C++   Create PDLT bank.
C
      NPDLT = NDDLT
      LEN = LMHLEN + LPDLTA * NDDLT
      CALL AUBOS('PDLT',0,LEN, KPDLT,IGARB)
      CALL BLIST(IW,'S+','PDLT')
      IF (IGARB.GE.2) THEN
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KDDLT = NLINK('DDLT',0)
      ENDIF
      IW(KPDLT+LMHCOL) = LPDLTA
      IW(KPDLT+LMHROW) = NDDLT
C
C++   Fill PDLT bank.
C
      DO 100 I=1,NDDLT
         IW(KROW(KPDLT,I)+JPDLPA) = ITABL(KDDLT,I,JDDLPA)
         IW(KROW(KPDLT,I)+JPDLJT) = ITABL(KDDLT,I,JDDLJT)
         RW(KROW(KPDLT,I)+JPDLPI) = FLOAT(ITABL(KDDLT,I,JDDLPI))/AFACTM
         RW(KROW(KPDLT,I)+JPDLPE) = FLOAT(ITABL(KDDLT,I,JDDLPE))/AFACTM
         IW(KROW(KPDLT,I)+JPDLVJ) = ITABL(KDDLT,I,JDDLVJ)

         IW(KROW(KPDLT,I)+JPDLFR) = ITABL(KDDLT,I,JDDLFR)
  100 CONTINUE
C
      RETURN
      END
