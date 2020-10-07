      LOGICAL FUNCTION TCHKHV(IRUN,IEVT,IFLG)
C
C-----------------------------------------------------------------------
C! Check TPC high voltage status for dE/dx purposes
C
C  R. Johnson    22-6-90
C  F.Ranjard - 26-4-94
C     get the high voltage word through ALTRHV
C  Input:   IRUN    /I    Current run number
C           IEVT    /I    Current event number
C  Output:  TCHKHV  /L    .TRUE. if high voltage is (believed to be) on
C           IFLG    /I    0 = test was made on dE/dx HV bit
C                         1 = test was made on TPC tracking HV bit
C                         2 = no test was made (banks not found)
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
      PARAMETER(JTDBID=1,JTDBVR=2,JTDBSI=4,LTDBSA=4)
      PARAMETER(JREVDS=1,JREVFE=2,JREVNE=4,JREVSB=6,JREVTI=7,JREVRB=8,
     +          JREVEC=10,LREVHA=10)
C
      SAVE
      INTEGER AGETDB, ALTRHV
      LOGICAL FIRST
      DATA FIRST/.TRUE./
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
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NREVH=NAMIND('REVH')
        NTDBS=NAMIND('TDBS')
        NXTCN=NAMIND('XTCN')
        IRLST=-1
      ENDIF
C
      IF (IRUN.NE.IRLST) THEN
        IRLST=IRUN
        IRET=AGETDB('TDBS',IRUN)
      ENDIF
      TCHKHV=.FALSE.
      IFLG=2
C
      KREVH=IW(NREVH)
      IF (KREVH.EQ.0) THEN
        IRET = ALTRHV(ISTAT)
        IF (IRET.EQ.0) RETURN
      ELSE
        ISTAT=ITABL(KREVH,1,JREVDS)
      ENDIF
      KTDBS=IW(NTDBS)
      IF (KTDBS.EQ.0) RETURN
C
      DO 100 I=1,LROWS(KTDBS)
        IR1=ITABL(KTDBS,I,JTDBVR)
        IF (IR1.GT.IRUN) GO TO 100
        IR2=ITABL(KTDBS,I,JTDBVR+1)
        IF (IR2.LT.IRUN) GO TO 100
C
C++     At least one sector is bad.  Do not check the dE/dx bit
C++     but only the other HV bit.  It would be nice to look at
C++     the slow control info at this point, but that will be
C++     difficult to implement because it is out of sinc with the
C++     event records.
C
        IF (IRUN.LT.4800) THEN
          IBTTP=IBITS(ISTAT,4,1)
        ELSE
          IBTTP=IBITS(ISTAT,15,1)
        ENDIF
        IF (IBTTP.EQ.1) THEN
          TCHKHV=.TRUE.
        ENDIF
        IFLG=1
        RETURN
  100 CONTINUE
C
C++   Did not find any bad sectors, so check the dE/dx bit
C
      IBTTP=IBITS(ISTAT,4,1)
      IF (IBTTP.EQ.1) TCHKHV=.TRUE.
      IFLG=0
C
      RETURN
      END
