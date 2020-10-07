      FUNCTION TDXERR(NS,TL,IER)
C
C-------------------------------------------------------------------
C! Get expected relative dE/dx error
CKEY DEDX TPC ERROR
C  R. Johnson  9-2-90
C
C  Input:   NS      /I     Number of dE/dx samples before truncation
C           TL      /R     Track length
C  Output:  TDXERR  /R     Error on the dE/dx (units 1=minion)
C           IER     /I     Non-zero if database banks not found
C
C----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JTC4ID=1,JTC4VR=2,JTC4MN=4,JTC4MX=5,JTC4RP=6,JTC4IP=13,
     +          LTC4XA=22)
C
      LOGICAL FOUND
C
C - set necessary data for GTDBBK
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2, LIST*4
      PARAMETER (DET='TP', LIST='TC4X')
      DATA IROLD/0/
C
      DATA NTC4X/0/
      DATA WIRNT/0.4/
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
      IF (NS.LE.0) THEN
        CALL ALTELL(' TDXERR: NS=0.  Abort to avoid divide by 0',0,
     &              'RETURN')
        TDXERR=.2
      ENDIF
      IF (NTC4X.EQ.0) THEN
        NTC4X=NAMIND('TC4X')
      ENDIF
C
C++   Look for the calibration constants
C! Get banks from DB depending on run and setup code
C
      CALL ABRUEV (IRUN,IEVT)
      IRET = 0
      IF (IRUN.NE.IROLD) THEN
        IROLD = IRUN
        IF (IRUN.LE.2000) THEN
           ITP = GTSTUP (DET,IRUN)
        ELSE
           ITP = IRUN
        ENDIF
        IRET= ALGTDB(JUNIDB(0),LIST,-ITP)
      ENDIF
C
      KTC4X=IW(NTC4X)
      IF (KTC4X.EQ.0) THEN
          IER=4
          GO TO 999
      ENDIF
C
C++   Get data-base constants for resolution parameterization
C
      DX=TL/FLOAT(NS)
      SMPL=ALOG(DX/WIRNT)
      DO 34 I=1,LROWS(KTC4X)
        IF (SMPL.GE.RTABL(KTC4X,I,JTC4MN)
     &         .AND. SMPL.LE.RTABL(KTC4X,I,JTC4MX)) THEN
          IB=I
          GO TO 35
        ENDIF
   34 CONTINUE
      CALL ALTELL(' TDXERR: sample length out of range of bank TC4X.',0,
     &            'RETURN')
      IB=1
   35 CONTINUE
C
      RFLAG= RTABL(KTC4X,IB,JTC4RP+6)
      IF (RFLAG.EQ.0. .AND. IRUN.GT.2000) THEN
C
C++     22 July 1991:
C++     Parameterization for calibration used on 89/90 data.
C++     This will no longer be used if 89/90 data are reprocessed.
C
        A= RTABL(KTC4X,IB,JTC4RP+3)
        B= RTABL(KTC4X,IB,JTC4RP+4)
        ARG= A/FLOAT(NS) + B
        IF (ARG.LT.0.) THEN
          CALL ALTELL(' TDXERR: SQRT of negative number. '//
     &             'Bad calibration consts in bank TC4X?',0,'RETURN')
          TDXERR=0.055
        ELSE
          TDXERR=SQRT(ARG)
        ENDIF
      ELSE
C
C++     This is the prefered formula, which takes into account both
C++     the sample-length dependence and the number of wires
C
        PNORM= RTABL(KTC4X,IB,JTC4RP)
        PPOW1= RTABL(KTC4X,IB,JTC4RP+1)
        PPOW2= RTABL(KTC4X,IB,JTC4RP+2)
C
        TDXERR= PNORM*(DX**PPOW2)*((FLOAT(NS))**PPOW1)
      ENDIF
C
      IER=0
  999 CONTINUE
      RETURN
      END
