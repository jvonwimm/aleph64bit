      SUBROUTINE TIDHYP(ITK,FIELD,N,RMASS,Q,RI,NS,TL,RIEXP,SIGMA,IER)
C
C----------------------------------------------------------------------
C! Particle ID hypothesis from TPC dE/dx
C! This is a user interface for dE/dx analysis.
C!
C!    Author:  R. Johnson    23-09-88
C!    Modified:S. Haywood    07-06-90
C!    Modified:S. Haywood    03-03-93
C!
C!    Input:   ITK      /I       Track number in FRFT
C!             FIELD    /R       Magnetic field (sign doesn't matter)
C!             N        /I       Number of hypothesis to try
C!             RMASS(n) /R       Mass hypotheses
C!             Q(n)     /R       Charge hypotheses (sign doesn't matter)
C!    Output:  RI       /R       Measured ionization (1.O=minion, Q=1)
C!             NS       /R       Number of useful wire samples on track
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
C!         TEXS            for dE/dx information
C!  Input calibration banks:
C!         TC1X, TC2X, and TC3X
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
#ifndef DOC
C
#include "alcons.h"
#include "bcs.h"
#include "frftjj.h"
#include "tc3xjj.h"
C
      DIMENSION RMASS(N),Q(N),RIEXP(N),SIGMA(N)
      LOGICAL FOUND
C
C - set necessary data for GTDBBK
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2, LIST*4
      PARAMETER (DET='TP', LIST='TC3X')
      DATA IROLD/0/
C
      DATA NFRFT, NTEXS, NTC3X /3*0/
C
C++   Spacing between sense wires in the TPC in cm
C
      DATA TWRSP/0.4/
C
#include "bmacro.h"
C
      IF (NFRFT.EQ.0) THEN
        NFRFT=NAMIND('FRFT')
        NTEXS=NAMIND('TEXS')
        NTC3X=NAMIND('TC3X')
      ENDIF
C
C++   Look for the necessary input banks
C
      KFRFT=IW(NFRFT)
      KTEXS=IW(NTEXS)
C
C++   Get the TC3X calibration bank
C
#include "gtdbbk.h"
      KTC3X = IW(NTC3X)
      IF (KTC3X.EQ.0) THEN
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
        CALL TMDEDX(ITK,RI,RSIG,NS,TL,IER)
        IF (IER.NE.0) GO TO 999
C
C++     Apply the TC3X normalization correction here.
C
        RI=RI*RTABL(KTC3X,1,JTC3NR)
      IF (NS.GT.0 .AND. TL.GT.0.) THEN
        SMP=TL/FLOAT(NS)
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
        CALL TXDEDX(BG,EBG,Q(I),RSIG,SMPL,RIEXP(I),SIGMA(I),IER)
        IF (IER.NE.0) THEN
          GO TO 999
        ENDIF
  300 CONTINUE
      IER=0
C
  999 CONTINUE
      RETURN
      END
#endif
