      SUBROUTINE MINPSC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill  bank PMSC for Alpha   .
C
C     Author: Agnieszka Jacholkowska    30-Sep-94
C
C     Input  : DMSC bank
C     Output : PMSC bank
C
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
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
      PARAMETER(JDMSC1=1,LDMSCA=1)
      PARAMETER(JPMSC1=1,LPMSCA=1)
C
      LOGICAL FIRST,XFRID,PACK
      DATA FIRST / .TRUE. /
C
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
C++   Pick up DMSC bank.
C
      KDMSC = NLINK('DMSC',0)
      IF(KDMSC.LE.0) RETURN
C
C++   Identify the number of tracks.
C++   If no tracks, return without creating PMSC bank.
C
      NDMSC = LROWS(KDMSC)
      IF(NDMSC.LE.0) RETURN
      NPMSC = NDMSC
      IR = IW(KDMSC-2)
      IRM = IR
      IF(IR.EQ.3) IRM = 0
C
C++   Create the PMSC bank.
C
      LEN = LMHLEN + LPMSCA * NPMSC
      CALL AUBOS('PMSC',IRM,LEN, KPMSC,IGARB)
      CALL BLIST(IW,'S+','PMSC')
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINPSC: Cannot create PMSC bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDMSC = NLINK('DMSC',IR)
      ENDIF
      IW(KPMSC+LMHCOL) = LPMSCA
      IW(KPMSC+LMHROW) = NPMSC
C
C++   Loop over the DMSC bank and fill the PMSC bank.
C
      DO 100 I=1,NDMSC
C
C++      Track parameters.
C
         RW(KROW(KPMSC,I)+JPMSC1) = FLOAT(ITABL(KDMSC,I,JDMSC1))/AFACTM

  100 CONTINUE
C
      RETURN
      END
