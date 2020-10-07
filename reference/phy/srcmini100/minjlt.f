      SUBROUTINE MINJLT
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Reconstructed tagged lepton bank DDLT for Mini-DST.
C
C     Author: Agnieszka Jacholkowska    14-Nov-94
C
C     Input  : PLJT bank
C     Output : DLJT bank
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
C++   Pick up PLJT bank.
C
      KPLJT = NLINK('PLJT',0)
      IF (KPLJT.LE.0) RETURN
      NPLJT = LROWS(KPLJT)
      IF (NPLJT.LE.0) RETURN
C
C++   Create the DLJT bank.
C
      NDLJT = NPLJT
      LEN = LMHLEN + LDLJTA * NDLJT
      CALL AUBOS('DLJT',0,LEN, KDLJT,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINJLT: Cannot create DLJT bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KPLJT = NLINK('PLJT',0)
      ENDIF
      IW(KDLJT+LMHCOL) = LDLJTA
      IW(KDLJT+LMHROW) = NDLJT
c     WRITE(IW(6),'('' MINJLT: DLJT bank lifted'')')
c     PRINT *, LDLJTA, NPLJT, NDLJT, LMHROW, IW(KDLJT+LMHROW)
C
C++   Loop over PLJT storing information in DLJT.
C
      DO 100 I=1,NDLJT
         IW(KROW(KDLJT,I)+JDLJPX) = NINT(AFACTM * RTABL(KPLJT,I,JPLJPX))
         IW(KROW(KDLJT,I)+JDLJPY) = NINT(AFACTM * RTABL(KPLJT,I,JPLJPY))
         IW(KROW(KDLJT,I)+JDLJPZ) = NINT(AFACTM * RTABL(KPLJT,I,JPLJPZ))
         IW(KROW(KDLJT,I)+JDLJPE) = NINT(AFACTM * RTABL(KPLJT,I,JPLJPE))
         IW(KROW(KDLJT,I)+JDLJNO) = ITABL(KPLJT,I,JPLJNO)
  100 CONTINUE
c     PRINT *, LDLJTA, NPLJT, NDLJT, LMHROW, IW(KDLJT+LMHROW)
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DLJT')
C
      RETURN
      END
