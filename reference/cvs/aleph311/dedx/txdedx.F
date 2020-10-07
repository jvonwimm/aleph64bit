      SUBROUTINE TXDEDX(BG,SBG,Q,RSIG,SMPL,DEDX,SDEDX,IER)
C
C-----------------------------------------------------------------------
C! Expected dE/dx of relativistic particle
CKEY DEDX PARTICLE
C!    Author:  R. Johnson    17-06-87
C!    Modified: Z. Feng      26-11-92 modify the power term to general purpose
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
#ifndef DOC
#include "bcs.h"
#include "tc4xjj.h"
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
#include "bmacro.h"
C ------------------------------------------------------------------
      IF (NTC4X.EQ.0) THEN
        NTC4X=NAMIND('TC4X')
      ENDIF
C
C++   Look for the calibration constants
#include "gtdbbk.h"
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
#endif
