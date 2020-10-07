      INTEGER FUNCTION ALKLUN (LUVER,LULMD)
C -------------------------------------------------------------------
CKEY ALEF LUND KLUN / USER
C - B.Bloch-Devaux 900926
C! Build Lund run header KLUN
C - Input  :  LUVER   = JETSET version number (packed as 703 for 7.3)
C             LULMD   = JETSET last date of change (as YYMMDD)
C - Output :  ALKLUN  = KLUN bank index
C                       0 means not enough space to book the bank
C  ---------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JKLUVN=1,JKLULM=2,LKLUNA=2)
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
C - Book the bank 'KLUN'
      CALL AUBOS ('KLUN',0,LMHLEN+LKLUNA,JKLUN,IGARB)
      IF (JKLUN.EQ.0) GOTO 999
      IW(JKLUN+LMHCOL) = LKLUNA
      IW(JKLUN+LMHROW) = 1
      CALL BKFMT ('KLUN','I')
C
C - fill the  row
      KKLUN = JKLUN + LMHLEN
      IW(KKLUN+JKLUVN) = LUVER
      IW(KKLUN+JKLULM) = LULMD
C
 999  CONTINUE
      ALKLUN = JKLUN
      END
