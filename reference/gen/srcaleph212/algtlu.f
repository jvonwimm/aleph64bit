      INTEGER FUNCTION  ALGTLU (LUVER,LULMD)
C -------------------------------------------------------------------
CKEY ALEF LUND VERSION KLUN KINGAL / USER
C - B.Bloch-Devaux 900926
C! Finds the JETSET version number used for generated events
C   Returns  ALGTLU = 1  if the file WAS  generated using JETSET
C                   = 0        #     WAS NOT            #
C - Output :  LUVER   = JETSET version number ( 703 FOR 7.3 VERSION )
C             LULMD   = JETSET date of last modification
C                          (900918 is 18 September 1990)
C Input banks : KLUN ,  KRUN
C ----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JKLUVN=1,JKLULM=2,LKLUNA=2)
      PARAMETER(JKRUGI=1,JKRUNO=2,JKRURT=3,JKRUFS=15,JKRUSS=16,
     +          LKRUNA=16)
C   Generators below this ID never used JETSET!
      PARAMETER (IGEN = 3002)
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
C ----------------------------------------------------------------------
        LUVER =0
        LULMD =0
        ALGTLU=0
C  Get Run header bank
        JKRUN=NLINK('KRUN',0)
        IF (JKRUN.EQ.0) GO TO 999
        ILUND =ITABL(JKRUN,1,JKRUGI)
        IF (ILUND.GT.IGEN) GO TO 999
        ALGTLU=1
        LUVER =603
        LULMD =870210
        JKLUN=NLINK('KLUN',0)
        IF (JKLUN.EQ.0) GO TO 999
        LUVER =ITABL(JKLUN,1,JKLUVN)
        LULMD =ITABL(JKLUN,1,JKLULM)
C
 999    RETURN
        END
