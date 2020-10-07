      FUNCTION TPXERR(NAME,XNS,TL,IER)
C-------------------------------------------------------------------
C! Get expected relative dE/dx error
CKEY DEDX TPC ERROR
C  R. Johnson  9-2-90
C  D. Casper  14-6-95  Create TPXERR from TDXERR
C
C  Input:   NAME    /A     'WIRE' or 'PAD'
C           XNS     /R     Number of dE/dx samples before truncation
C           TL      /R     Track length
C  Output:  TPXERR  /R     Error on the dE/dx (units 1=minion)
C           IER     /I     Non-zero if database banks not found
C
C  Input banks:
C     WIRE dE/dx - TC4X (calibration)
C     PAD  dE/dx - TP4X (calibration)
C     TC4X and TP4X have the same HAC parameters
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
      INTEGER JTP4MN,JTP4MX,JTP4RP,JTP4IP,LTP4XA
      PARAMETER(JTP4MN=1,JTP4MX=2,JTP4RP=3,JTP4IP=10,LTP4XA=19)
C
      CHARACTER*(*) NAME
      CHARACTER*4 TNAM, CHAINT
      LOGICAL FOUND
C
C - set necessary data for GTDBBK
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2, LIST*8
      PARAMETER (DET='TP', LIST='TC4XTP4X')
      DATA IROLD/0/
C
      DATA NTC4X, NTP4X /2*0/
      DATA WIRNT/0.4/
      DATA PADNT/3.0/
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
      IF (XNS.LE.0.) THEN
        CALL ALTELL(' TPXERR: XNS=0. Abort to avoid divide by 0',0,
     &              'RETURN')
        TPXERR=.2
      ENDIF
C
      IF (NTC4X.EQ.0) THEN
        NTP4X=NAMIND('TP4X')
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
C
C - WIRE or PAD
C
      IF (NAME(1:1) .EQ. 'P') THEN
         NTNAM = NTP4X
         TNANT = PADNT
         JTNARP= JTP4RP
         JTNAMN= JTP4MN
         JTNAMX= JTP4MX
      ELSE
         NTNAM = NTC4X
         TNANT = WIRNT
         JTNARP= JTC4RP
         JTNAMN= JTC4MN
         JTNAMX= JTC4MX
      ENDIF
C
      KTNAM=IW(NTNAM)
      IF (KTNAM.EQ.0) THEN
          IER=4
          GO TO 999
      ENDIF
C
C++   Get data-base constants for resolution parameterization
C
      DX=TL/XNS
      SMPL=ALOG(DX/TNANT)
      DO 34 I=1,LROWS(KTNAM)
        IF (SMPL.GE.RTABL(KTNAM,I,JTNAMN)
     &         .AND. SMPL.LE.RTABL(KTNAM,I,JTNAMX)) THEN
          IB=I
          GO TO 35
        ENDIF
   34 CONTINUE
      TNAM = CHAINT (IW(KTNAM-3))
      CALL ALTELL(' TPXERR: sample length out of range of bank '//TNAM,
     &            0,'RETURN')
      IB=1
   35 CONTINUE
C
      RFLAG= RTABL(KTNAM,IB,JTNARP+6)
      IF (RFLAG.EQ.0. .AND. IRUN.GT.2000) THEN
C
C++     22 July 1991:
C++     Parameterization for calibration used on 89/90 data.
C++     This will no longer be used if 89/90 data are reprocessed.
C
        A= RTABL(KTNAM,IB,JTNARP+3)
        B= RTABL(KTNAM,IB,JTNARP+4)
        ARG= A/XNS + B
        IF (ARG.LT.0.) THEN
          TNAM = CHAINT (IW(KTNAM-3))
          CALL ALTELL(' TPXERR: SQRT of negative number. '//
     &         'Bad calibration consts in bank '//TNAM,0,'RETURN')
          TPXERR=0.055
        ELSE
          TPXERR=SQRT(ARG)
        ENDIF
      ELSE
C
C++     This is the prefered formula, which takes into account both
C++     the sample-length dependence and the number of wires
C
        PNORM= RTABL(KTNAM,IB,JTNARP)
        PPOW1= RTABL(KTNAM,IB,JTNARP+1)
        PPOW2= RTABL(KTNAM,IB,JTNARP+2)
C
        TPXERR= PNORM*(DX**PPOW2)*(XNS**PPOW1)
      ENDIF
C
      IER=0
  999 CONTINUE
      RETURN
      END
