      FUNCTION TBTBLK(BGL,Q,SMPL,IER)
C
C----------------------------------------------------------------------
C! Bethe-Bloch parameterization of TPC dE/dx for analysis
CKEY DEDX TPC BETHE
C!   Created by Robert P. Johnson     22-AUG-1988
C!
C!   Inputs    : BGL       /R      log10(beta*gamma)
C!               Q         /R      particle charge
C!               SMPL      /R      log(sample length/4mm)
C!   Outputs   : TBTBLK    /R      dE/dx with minion=1.0
C!                                 Set to zero if Q=0 or if the
C!                                 calibration cannot be found.
C!                                 set to zero if <0.
C!               IER       /I      Error return=0 for success
C!                                  4= cannot find calibration bank TC4X
C!                                  5= stupid result TBTBLK<0.
C!
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
      REAL LN10
C
C - set necessary data for GTDBBK
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2, LIST*4
      PARAMETER (DET='TP', LIST='TC4X')
      DATA IROLD/0/
C
      DATA MM/3/,RM/3./,TL10/4.605170186/
      DATA NTC4X , NASIM /2*0/
      DATA LN10/2.30258509/
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
C++   Neutral tracks don't have any dE/dx
C
      IF (Q.EQ.0.) THEN
        IER=0
        TBTBLK=0.
        GO TO 999
      ENDIF
C
      IF (NTC4X.EQ.0) THEN
        NTC4X = NAMIND('TC4X')
      ENDIF
C
C
C++   Find the bank of calibration constants
C
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
      KTC4X = IW(NTC4X)
      IF (KTC4X.EQ.0) THEN
          IER=4
          TBTBLK=0.
          GO TO 999
       ENDIF
C
      Q2=Q**2
      IF (RTABL(KTC4X,1,JTC4IP+5).EQ.0.)THEN
C
C++     Opal formula
C
        IB= 1
        XI= RTABL(KTC4X,IB,JTC4IP)
        RK= RTABL(KTC4X,IB,JTC4IP+1)
        AA= RTABL(KTC4X,IB,JTC4IP+2)
        XA= RTABL(KTC4X,IB,JTC4IP+3)
        P=  RTABL(KTC4X,IB,JTC4IP+4)
        X0= XA - (AA*(TL10/(RM*AA))**(RM/(RM-1.0)))/TL10
        X1= X0 + (TL10/(RM*AA))**(1./(RM-1.0))
        BG= 10.0**BGL
        B2= BG**2/(1.0+BG**2)
        BETA= SQRT(B2)
        BP= BETA**P
        IF (BGL.LT.X0) THEN
          DEL=0.
        ELSEIF (BGL.LT.X1) THEN
          DEL= TL10*(BGL-XA)+AA*(X1-BGL)**MM
        ELSE
          DEL= TL10*(BGL-XA)
        ENDIF
        TBTBLK= (XI*Q2/BP)*(RK+TL10*BGL-BP-DEL)
        IER=0
      ELSE
        DO 34 I=1,LROWS(KTC4X)
          IF (SMPL.GE.RTABL(KTC4X,I,JTC4MN)
     &           .AND. SMPL.LE.RTABL(KTC4X,I,JTC4MX)) THEN
            IB=I
            GO TO 35
          ENDIF
   34   CONTINUE
        CALL ALTELL(' TBTBLK: Sample length out of range of bank TC4X.',
     &              0,'RETURN')
        IB=1
   35   CONTINUE
        XI=RTABL(KTC4X,IB,JTC4IP)
        P= RTABL(KTC4X,IB,JTC4IP+1)
        RK=RTABL(KTC4X,IB,JTC4IP+2)
        X0=RTABL(KTC4X,IB,JTC4IP+3)
        X1=RTABL(KTC4X,IB,JTC4IP+4)
        A3=RTABL(KTC4X,IB,JTC4IP+5)*1.0E-3
        A4=RTABL(KTC4X,IB,JTC4IP+6)*1.0E-6
        A5=RTABL(KTC4X,IB,JTC4IP+7)*1.0E-7
        A6=RTABL(KTC4X,IB,JTC4IP+8)*1.0E-8
        A7=RTABL(KTC4X,IB,JTC4IP+9)*1.0E-9
C
        X=LN10*BGL
        ETA= 10.0**BGL
        BETA = ETA/(SQRT(1.+ETA**2))
        BP=BETA**P
        T= X0-X1
        SUMD= (((((7.*A7*T+6.*A6)*T+5.*A5)*T+4.*A4)*T+3.*A3)*T)*T
        A2= -(1.+0.5*SUMD)/T
        SUM= ((((((A7*T+A6)*T+A5)*T+A4)*T+A3)*T+A2)*T)*T
        XA= X0 + 0.5*SUM
        IF (X.LT.X0) THEN
          DEL=0.
        ELSEIF (X.LT.X1) THEN
          T=X-X1
          SUM= ((((((A7*T+A6)*T+A5)*T+A4)*T+A3)*T+A2)*T)*T
          DEL= 2.*(X-XA) + SUM
        ELSE
          DEL= 2.*(X-XA)
        ENDIF
        TBTBLK= (XI*Q2/BP)*(RK+2.*X-BP-DEL)
        IF (TBTBLK .LT. 0.) THEN
           TBTBLK = 0.
           IER =5
        ENDIF
C
      ENDIF
C
  999 RETURN
      END
