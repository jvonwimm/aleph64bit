      INTEGER FUNCTION ALKJOB (IDBV,IDBD)
C ------------------------------------------------------------------
C - B.Bloch-Devaux 880720
C! Book and fill kine job header KJOB
C - Input :  IDBV   = DATA BASE version #
C            IDBD   = DATA BASE last mod date
C - Output : ALKJOB = KJOB bank index
C                     0 means not enough space to book the bank
C
C ------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
CKEY KINE KINGAL HAC
      PARAMETER(JKEVRN=1,JKEVNT=2,JKEVNV=3,JKEVPI=4,JKEVWT=5,JKEVSR=6,
     +          JKEVTR=7,LKEVHA=7)
      PARAMETER(JKHIHC=1,LKHISA=1)
      PARAMETER(JKJOJD=1,JKJOJT=2,JKJOAV=3,JKJODV=4,JKJODC=5,LKJOBA=5)
      PARAMETER(JKLIGN=1,LKLINA=1)
      PARAMETER(JKPOKI=1,JKPOHX=2,JKPOHY=3,JKPOHZ=4,LKPOLA=4)
      PARAMETER(JKRUGI=1,JKRUNO=2,JKRURT=3,JKRUFS=15,JKRUSS=16,
     +          LKRUNA=16)
      PARAMETER(JKVOVN=1,JKVOVM=2,LKVOLA=2)
C - ALEPHLIB   21.1   951208  17:30:39
      REAL ALEVER
      PARAMETER (ALEVER = 21.1)
C
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
C - Book the 'KJOB' bank
      CALL AUBOS ('KJOB',0,LMHLEN+LKJOBA,JKJOB,IGARB)
      IF (JKJOB.EQ.0) GOTO 999
      IW(JKJOB+LMHCOL) = LKJOBA
      IW(JKJOB+LMHROW) = 1
      CALL BKFMT ('KJOB','7I')
C
C - Get date and time
C
      CALL DATIME(JDAT,JTIM)
C
C - Fill 'KJOB'
C
      KKJOB = JKJOB + LMHLEN
      IW(KKJOB+1) = JDAT
      IW(KKJOB+2) = JTIM
      IALV = ALEVER*10.
      IW(KKJOB+3) = IALV
      IW(KKJOB+4) = IDBV
      IW(KKJOB+5) = IDBD
C
 999  CONTINUE
      ALKJOB = JKJOB
      END
