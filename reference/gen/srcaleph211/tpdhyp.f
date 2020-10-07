      SUBROUTINE TPDHYP(NAME,ITK,FIELD,N,RMASS,Q,RI,XNS,TL,RIEXP,SIGMA,
     +                  IER)
C
C----------------------------------------------------------------------
C! Particle ID hypothesis from TPC dE/dx
C! This is a user interface for dE/dx analysis.
C!
C!    Author:  R. Johnson    23-09-88
C!    Modified:S. Haywood    07-06-90
C!    Modified:S. Haywood    03-03-93
C!    Modified:D. Casper     14-06-95 add pad dedx
C!
C!    Input:   NAME     /A       'PAD' or 'WIRE'
C!             ITK      /I       Track number in FRFT
C!             FIELD    /R       Magnetic field (sign doesn't matter)
C!             N        /I       Number of hypothesis to try
C!             RMASS(n) /R       Mass hypotheses
C!             Q(n)     /R       Charge hypotheses (sign doesn't matter)
C!    Output:  RI       /R       Measured ionization (1.O=minion, Q=1)
C!             XNS      /R       Number of useful wire samples on track
C!             TL       /R       Useful length of the track (cm)
C!             RIEXP(n) /R       Expected ionization for the given
C!                               mass hypothesis (1.0=minion, Q=1)
C!             SIGMA(n) /R       Sigma of dE/dx measurement error,
C!                               including the momentum error.
C!                               Note that one can calculate a
C!                               chi-squared with 1 d.o.f. as:
C!                               chi2 = ((RI-RIEXP)/SIGMA)**2
C!             IER      /I       Error return= 0 for success.
C!                               1= can't find track bank
C!                               2= can't find dE/dx bank
C!                               3= track has no dE/dx information
C!                               4= can't find calibration banks
C!                                  TC1X, TC2X, and/or TC3X
C!                               6= no valid dE/dx calibration exists
C!                                  for this run
C!  Input data banks:
C!         FRFT            for tracking information
C!  Input calibration banks:
C!         TC1X, TC2X, and
C!         TC3X  for WIRE dE/dx , TP3X for PAD dE/dx
C!     JTNANR is set to JTC3NR or JTP3NR (overall normalisation corr.)
C!
C!  Comments
C!  ========
C!  This interface is to be used for analysis of dE/dx information
C!  contained on POT, DST, or MDST data files.  One provides the
C!  track number and one or more particle hypotheses, and the
C!  program returns the corresponding measured dE/dx, expected
C!  dE/dx, and the estimated 1 sigma measurement error.  All
C!  calibration factors valid for the current run are applied.
C!  The program also returns the number of dE/dx samples of the track
C!  and the effective length of the track.
C!
C-------------------------------------------------------------------
C
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JTC3ID=1,JTC3VR=2,JTC3NR=4,JTC3PR=5,LTC3XA=11)
      INTEGER JTP3NR,LTP3XA
      PARAMETER(JTP3NR=1,LTP3XA=1)
C
      CHARACTER*(*) NAME
      DIMENSION RMASS(N),Q(N),RIEXP(N),SIGMA(N)
      LOGICAL FOUND
C
C - set necessary data for GTDBBK
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2, LIST*8
      PARAMETER (DET='TP', LIST='TC3XTP3X')
      DATA IROLD/0/
C
      DATA NFRFT, NTC3X, NTP3X /3*0/
C
C++   Spacing between sense wires in the TPC in cm
C
      DATA TWRSP/0.4/
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
      IF (NFRFT.EQ.0) THEN
        NFRFT=NAMIND('FRFT')
        NTC3X=NAMIND('TC3X')
        NTP3X=NAMIND('TP3X')
      ENDIF
C
C++   Look for the necessary input banks
C
      KFRFT=IW(NFRFT)
C
C++   Get the calibration banks
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
C
C - wire or pad
C
      IF (NAME(1:3) .EQ. 'PAD') THEN
         NTNAM = NTP3X
         JTNANR= JTP3NR
      ELSE
         NTNAM = NTC3X
         JTNANR= JTC3NR
      ENDIF
C
      KTNAM = IW(NTNAM)
      IF (KTNAM.EQ.0) THEN
         IER = 4
         GOTO 999
      ENDIF
C
C++   Get the track momentum and error on momentum
C
        RI=RTABL(KFRFT,ITK,JFRFIR)
        IF (RI.NE.0.) THEN
          RAD=1./RI
        ELSE
          RAD=1.0E20
        ENDIF
        PT=RAD*CLGHT*FIELD/100000.
        TANL=RTABL(KFRFT,ITK,JFRFTL)
        SECL=SQRT(1.0+TANL**2)
        SINL=TANL/SECL
        P=ABS(PT)*SECL
        DPDRI= -P*RAD
        DPDTL= PT*SINL
        SRI=RTABL(KFRFT,ITK,JFRFEM)
        STL=RTABL(KFRFT,ITK,JFRFEM+2)
        SCOR=RTABL(KFRFT,ITK,JFRFEM+1)
        SGP=SQRT(AMAX1(0.,(DPDRI*SRI)*DPDRI + (DPDTL**2)*STL
     &                        + 2.0*(DPDRI*DPDTL)*SCOR))
C
C++   Get the particle's measured dE/dx and its relative error
C
        CALL TMPDDX(NAME,ITK,RI,RSIG,XNS,TL,IER)
        IF (IER.NE.0) GO TO 999
C
C++     Apply the TNAM normalization correction here.
C
        RI=RI*RTABL(KTNAM,1,JTNANR)
      IF (XNS.GT.0 .AND. TL.GT.0.) THEN
        SMP=TL/XNS
        SMPL=ALOG(SMP/TWRSP)
      ELSE
        SMPL=0.
      ENDIF
C
C++   Loop over the N mass hypotheses
C
      DO 300 I=1,N
C
C++     Get beta*gamma of the particle and its error
C
        BG=P/RMASS(I)
        EBG=SGP/RMASS(I)
C
C++     Get the expected dE/dx and resolution, including momentum
C++     contribution.
C
        CALL TXPDDX(NAME,BG,EBG,Q(I),RSIG,SMPL,RIEXP(I),SIGMA(I),IER)
        IF (IER.NE.0) THEN
          GO TO 999
        ENDIF
  300 CONTINUE
      IER=0
C
  999 CONTINUE
      RETURN
      END
