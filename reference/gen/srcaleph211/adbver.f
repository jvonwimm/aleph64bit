      SUBROUTINE ADBVER (IVERS,IDATE)
C.---------------------------------------------------------------------
CHEY ALEF DBASE STC LTC VERSION / USER
C  F.Ranjard - 881110
C              900111 : introduce LTC version
C! Return  DAF version# and date of last change
C
C  Output: IVERS   = DAF version version
C          IDATE   = DAF date of last change
C
C.---------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JADBVN=1,JADBDC=2,LADBSA=2)
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
C --------------------------------------------------------------------
      LDBAS = JUNIDB(0)
C - get version # and date of last change of the DAF
C
      IND = MDARD (IW,LDBAS,'ADBS',0)
      IF (IND .EQ. 0) THEN
         IDATE = 0
         IVERS = 0
      ELSE
         IDATE = ITABL(IND,1,JADBDC)
         IVERS = ITABL(IND,1,JADBVN)
      ENDIF
      LOUT = IW(6)
      IF (LOUT .GT. 0) THEN
         WRITE (LOUT,'(/1X,''+++ADBVER+++ ADBSCONS DAF version '',I5,
     &               3X,''date of last change '',I7,3X,''opened on'',
     &               '' unit#'',I3
     &          /)') IVERS,IDATE,LDBAS
      ENDIF
      RETURN
C
      END
