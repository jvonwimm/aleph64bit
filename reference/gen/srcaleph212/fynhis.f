      INTEGER FUNCTION FYNHIS (IOHIS)
C ---------------------------------------------------------
CKEY FYXX  / INTERNAL
C! Return the new history code
C - F.Ranjard - 881125
C - Input     : IOHIS  old history code
C - Output    : FYNHIS new history code
C - Called by : FYFKIN
C               assume that JDKNFO exists, use IHTYPE history type
C               to recalculate the history code properly.
C               returns the old history code in case the history type
C               is unknown, or a new history code cannot be computed.
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LWORDB = 65536)
      PARAMETER (MAXMCX = 1000, MAXMES = 500)
      COMMON /FYRELA/ IHTYPE,ICALTO
     &               ,ITRCAL(MAXMES),ICALTR(2,MAXMES)
     &               ,LTKNUM,LVXNUM,IMEATR,KINTRK,KINVER
     &               ,FSHDRO,FTKDRO,CUTRAC,BFLCLT,ENMINC
     &               ,INDKIN,INDVER,JDKEKS,JDVNFO,JDVOUT
     &               ,JDKOFN,JDKNFO
      LOGICAL FSHDRO,FTKDRO
      PARAMETER (LFXWBK = 7)
      INTEGER  JDFXWB(LFXWBK)
      EQUIVALENCE (JDFXWB(1),INDKIN)
      COMMON /FYCHAR/ ELISAD
      CHARACTER*48 ELISAD
      DATA ILUD/ 10000/
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
C ---------------------------------------------------------------
C
      FYNHIS = IOHIS
C
      IF (IHTYPE .EQ. 0) THEN
C -    LUND history code or NO history code
         IOMOM = MOD(IOHIS,ILUD)
         IF (IOMOM .NE. 0) THEN
            INEW = ITABL (JDKNFO,IOMOM,1)
            FYNHIS = INEW + ILUD*(IOHIS/ILUD)
         ENDIF
      ENDIF
C
C
C  special for HERWIG history code
C
      IF (IHTYPE .EQ. 1) THEN
         IOMOM = MOD(IOHIS,1000000)
         IOMOM2= IOMOM / 1000
         IOMOM = IOMOM - IOMOM2*1000
         IF (IOMOM .NE. 0) THEN
            INEW = ITABL (JDKNFO,IOMOM,1)
            FYNHIS = INEW + 1000000*(IOHIS/1000000)
            IF (IOMOM2.NE. 0) THEN
               INEW = ITABL (JDKNFO,IOMOM2,1)
               FYNHIS = INEW*1000 + FYNHIS
            ENDIF
         ENDIF
      ENDIF
C
      END
