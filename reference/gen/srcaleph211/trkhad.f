        SUBROUTINE TRKHAD (NGDTR,ECHRG)
C----------------------------------------------------------------------
CKEY EDIR TRACKS ENERGY
C! Returns number of good tracks and charged energy.
C! Good tracks:
C!      |D0| < 2cm, |Z0| < 10cm, >=4 TPC coord., |COS(THETA)|<0.95
C-
C   Input  : None
C   Output : NGDTR  = Number of good charged tracks
C            ECHRG  = Energy sum of NGDTR charged track
C-
C   Called by   : SELCAL,SELTRK
C   Calls  : QPTRCK
C   Input banks : PFRF,PFRT
C-
C                                   Author: Ed. Blucher     18-Oct-1989
C-----------------------------------------------------------------------
      SAVE
C --
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPFRIR=1,JPFRTL=2,JPFRP0=3,JPFRD0=4,JPFRZ0=5,JPFRAL=6,
     +          JPFREO=7,JPFREM=13,JPFRC2=28,JPFRNO=29,LPFRFA=29)
      PARAMETER(JPFRNV=1,JPFRNI=2,JPFRNE=3,JPFRNT=4,JPFRNR=5,LPFRTA=5)
C --
      DATA AMPI /0.13956755/
      DATA D0CUT,Z0CUT,NTPCO,ANGCUT/2.,10.,4,.95/
C --
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
C --
      NGDTR = 0
      ECHRG = 0.
C --
      KPFRF = IW(NAMIND('PFRF'))
      KPFRT = IW(NAMIND('PFRT'))
      IF(KPFRF.LE.0 .OR. KPFRT.LE.0) GOTO 999
C --
C   Loop over the charged tracks and selected good candidates
C --
      NTRK = LROWS(KPFRF)
      DO 10 IT = 1,NTRK
        D0 = ABS(RTABL(KPFRF,IT,JPFRD0))
        Z0 = ABS(RTABL(KPFRF,IT,JPFRZ0))
        NH = ITABL(KPFRT,IT,JPFRNT)
        IF(NH.LT.NTPCO .OR. D0.GT.D0CUT .OR. Z0.GT.Z0CUT) GOTO 10
        CALL QPTRCK(IT,PZ,PTOT)
        IF(PTOT.EQ.0.) GOTO 10
        IF(ABS(PZ/PTOT).GT.ANGCUT) GOTO 10
        ECHRG=ECHRG + SQRT(PTOT**2 + AMPI**2)
        NGDTR= NGDTR + 1
   10 CONTINUE
999   RETURN
      END
