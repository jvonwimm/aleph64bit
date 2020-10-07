      SUBROUTINE MINPJL
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill PLJT bank from DLJT.
C
C     Author: Agnieszka Jacholkowska    14-Nov-94
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
      PARAMETER(JPLJPX=1,JPLJPY=2,JPLJPZ=3,JPLJPE=4,JPLJNO=5,LPLJTA=5)
      PARAMETER(JDLJPX=1,JDLJPY=2,JDLJPZ=3,JDLJPE=4,JDLJNO=5,LDLJTA=5)

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
C++   Pick up DLJT bank.
C
      KDLJT = NLINK('DLJT',0)
      IF (KDLJT.LE.0) RETURN
      NDLJT = LROWS(KDLJT)
      IF (NDLJT.LE.0) RETURN
C
C++   Create PLJT bank.
C
      NPLJT = NDLJT
      LEN = LMHLEN + LPLJTA * NDLJT
      CALL AUBOS('PLJT',0,LEN, KPLJT,IGARB)
      CALL BLIST(IW,'S+','PLJT')
      IF (IGARB.GE.2) THEN
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KDLJT = NLINK('DLJT',0)
      ENDIF
      IW(KPLJT+LMHCOL) = LPLJTA
      IW(KPLJT+LMHROW) = NDLJT
C
C++   Fill PLJT bank.
C
      DO 100 I=1,NDLJT
         RW(KROW(KPLJT,I)+JPLJPX) = FLOAT(ITABL(KDLJT,I,JDLJPX))/AFACTM
         RW(KROW(KPLJT,I)+JPLJPY) = FLOAT(ITABL(KDLJT,I,JDLJPY))/AFACTM
         RW(KROW(KPLJT,I)+JPLJPZ) = FLOAT(ITABL(KDLJT,I,JDLJPZ))/AFACTM
         RW(KROW(KPLJT,I)+JPLJPE) = FLOAT(ITABL(KDLJT,I,JDLJPE))/AFACTM
         IW(KROW(KPLJT,I)+JPLJNO) = ITABL(KDLJT,I,JDLJNO)
  100 CONTINUE
C
      RETURN
      END
