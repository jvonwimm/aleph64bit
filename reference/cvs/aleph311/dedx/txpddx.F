      SUBROUTINE TXPDDX(NAME,BG,SBG,Q,RSIG,SMPL,DEDX,SDEDX,IER)
C-----------------------------------------------------------------------
C! Expected dE/dx of relativistic particle
CKEY DEDX PARTICLE
C!    Author:  R. Johnson    17-06-87
C!    Modified: Z. Feng      26-11-92 modify the power term to general purpose
C!              D. Casper    14-06-95 create TXPDDX from TXDEDX
C!
C!    Input:  NAME    /A     'PAD ' or 'WIRE'
C!            BG      /R     beta*gamma of the particle
C!            SBG     /R     Uncertainty in BG
C!            Q       /R     Particle charge
C!            RSIG    /R     Relative uncertainty of trunc mean, as
C!                           given by the routine TMPDDX
C!    Output: DEDX    /R     Energy loss relative to minimum ionizing
C!            SDEDX   /R     Estimate of uncertainty in DEDX
C!            IER     /I     Error return=0 for success
C!                               4= cannot find calibration bank
C!                               5= TBTBLP returns a negative value,
C!                                  TBTBLP is set to zero.
C!    Calibration bank:
C!       WIRE dE/dx - TC4X
C!       PAD  dE/dx - TP4X
C!
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
#include "tp4xjj.h"
C
      CHARACTER*(*) NAME
C
      PARAMETER (ALG10=2.30258509, EPS=0.005, RNORM=1.000)
C
C - set necessary data for GTDBBK
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2, LIST*8
      PARAMETER (DET='TP', LIST='TC4XTP4X')
      DATA IROLD/0/
      DATA NTC4X, NTP4X /2*0/
C
#include "bmacro.h"
C ------------------------------------------------------------------
      IF (NTC4X.EQ.0) THEN
        NTC4X=NAMIND('TC4X')
        NTP4X=NAMIND('TP4X')
      ENDIF
C
C++   Look for the calibration constants
#include "gtdbbk.h"
C
C - Wire or Pad
C
      IF (NAME(1:3) .EQ.'PAD') THEN
         NTNAM = NTP4X
         JTNARP= JTP4RP
      ELSE
         NTNAM = NTC4X
         JTNARP= JTC4RP
      ENDIF
C
      KTNAM=IW(NTNAM)
      IF (KTNAM.EQ.0) THEN
        IER=4
        GO TO 999
      ENDIF
C
C++   Get expected dE/dx as function of log10(beta*gamma)
C++   Also estimate the derivative (which need not be precise)
C
      BGLOG=ALOG10(BG)
      DEDX= TBTBLP(NAME,BGLOG,Q,SMPL,IER)*RNORM
      DEDXP= TBTBLP(NAME,BGLOG+EPS,Q,SMPL,IER)*RNORM
      DIDBG= (DEDXP-DEDX)/EPS
C
C++   Use the length, number of measurements, and DEDX itself to
C++   estimate the uncertainty on DEDX
C
      PPOW5=RTABL(KTNAM,1,JTNARP+5)
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
