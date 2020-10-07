      SUBROUTINE TXDEDX(BG,SBG,Q,RSIG,SMPL,DEDX,SDEDX,IER)
C
C-----------------------------------------------------------------------
C! Expected dE/dx of relativistic particle
CKEY DEDX PARTICLE
C!    Author:  R. Johnson    17-06-87
C!    Modified: Z. Feng      26-11-92 modify the power term to general p
C!
C!    Input:  BG      /R     beta*gamma of the particle
C!            SBG     /R     Uncertainty in BG
C!            Q       /R     Particle charge
C!            RSIG    /R     Relative uncertainty of trunc mean, as
C!                           given by the routine TMDEDX
C!    Output: DEDX    /R     Energy loss relative to minimum ionizing
C!            SDEDX   /R     Estimate of uncertainty in DEDX
C!            IER     /I     Error return=0 for success
C!                               4= cannot find calibration bank TC3X
C!                               5= TBTBLK returns a negative value,
C!                                  TBTBLK is set to zero.
C!    Description
C!    -----------
C!    This routine returns the most probable value of the
C!    dE/dx distribution expected for a TPC track of
C!    velocity beta, where beta*gamma= beta/SQRT(1-beta**2).
C!    Also returned is the 1-sigma uncertainty  on this most
C!    probable value.
C!
C!----------------------------------------------------------------------
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
C
      PARAMETER (ALG10=2.30258509, EPS=0.005, RNORM=1.000)
C
C - set necessary data for GTDBBK
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2, LIST*4
      PARAMETER (DET='TP', LIST='TC4X')
      DATA IROLD/0/
      DATA NTC4X/0/
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
C ------------------------------------------------------------------
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
C++   Get expected dE/dx as function of log10(beta*gamma)
C++   Also estimate the derivative (which need not be precise)
C
      BGLOG=ALOG10(BG)
      DEDX= TBTBLK(BGLOG,Q,SMPL,IER)*RNORM
      DEDXP= TBTBLK(BGLOG+EPS,Q,SMPL,IER)*RNORM
      DIDBG= (DEDXP-DEDX)/EPS
C
C++   Use the length, number of measurements, and DEDX itself to
C++   estimate the uncertainty on DEDX
C
      PPOW5=RTABL(KTC4X,1,JTC4RP+5)
      SDEDX= (DEDX**(1.0-PPOW5))*RSIG
C
C++   Fold in the contribution from the momentum uncertainty
C
      SBGLG= SBG/BG/ALG10
      SDEDX= SQRT(SDEDX**2 + (DIDBG*SBGLG)**2)
C
  999 CONTINUE
      RETURN
      END
