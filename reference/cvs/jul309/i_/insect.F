      SUBROUTINE INSECT(ITRY,LAYER,PAR,COV,TPHIS,THSCT,XPAR,ROAD,IWWIN,
     +IER)
C-----------------------------------------------------------------------
C!  Intersect TPC track with ITC layer.
C!
C!    Author      :- J. Sedgbeer 87/02/09
C!    Modified    :- J. Sedgbeer 88/06/22
C!    Modified    :- I. Tomalin  89/09/01
C!    Modified    :- P. Comas    94/08/18
C!    Modified    :- J.Sedgbeer  95/08/10 Add test for intersection.
C!                   Also tighten sin(A) test to not allow angles too
C!                   near 90 deg.
C!
C!   Input:
C!     ITRY   /I  : Type of search flag
C!     LAYER  /I4 : ITC layer number
C!     PARi   /R4 : TPC track parameters (sig*r0,tanl,phi0,d0,z0)
C!     COVi   /R4 : TPC Covariance matrix (as in TGFT bank)
C!     TPHIS  /R4 : Phi of TPC track at intersection with scatt. radius
C!     THSCT  /R4 : r.m.s. scat. angle of TPC track (proj. into r-phi)
C!     need commons /ITWICC/ for ITC geom.
C!                  /ITRKCC/ for tracking params and constants.
C!                  /IRESOL/ for resolution.
C!                  /ITRKER/ for tracking param.
C!     need  ALCONS  for pi etc.
C!
C!   Output:
C!     XPARi  /R4 : i=1  phi of  intersection
C!                  i=2  z of intersection
C!                  i=3  angle of track thro cell
C!     ROADi  /R4 : i=1  1 sigma uncertainty in XPAR(1) (cm)
C!                  i=2  1 sigma uncertainty in XPAR(2) (cm)
C!     IWWINi /I4 : i=1  lower bound of wire numbers inside road
C!                  i=2  upper bound of wire numbers inside road
C!     IER    /I4 : error flag   = 0  O.K.
C!                               = 1 no intersection
C!
C!   called by : IGICHA
C!   calls     : none
C!
C!   Libraries required: none
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
#include "alcons.h"
#include "itwicc.h"
#include "itrkcc.h"
#include "iresol.h"
#include "itrker.h"
C-----------------------------------------------------------------------
      DIMENSION PAR(*),COV(*),XPAR(*),ROAD(*),IWWIN(*)
C-----------------------------------------------------------------------
      IER=1
C
C Get intersection of track with ITC layer.
C
      R0    = PAR(1)
      RAD   = RWIRIT(LAYER)
      RADQ  = RAD*RAD
      D0    = PAR(4)
      D0Q   = D0*D0
C
C Check that track intersects layer
C
      IF (ABS(D0).GT.RAD .OR. ABS(2.0*R0-D0).LT.RAD) GOTO 999
C
      FACT1 = (RAD-D0)*(RAD+D0)
      FACT2 = 0.5*FACT1/(R0-D0)
      SINA  = (FACT2-D0)/RAD
C
C Limit angle to 89.9deg approx.
      IF(ABS(SINA).GE.0.999999) GOTO 999
      PHIT = PAR(3) + ASIN(SINA)
      PHIT = MOD(PHIT+TWOPI,TWOPI)
      XPAR(1) = PHIT
C
C Calculate the derivatives of PHIT w.r.t. 1/PAR(1), PAR(3) and PAR(4).
C First get the derivative of PHIT w.r.t. SINA and of SINA w.r.t.
C 1/PAR(1) and PAR(4). (N.B. COV is in terms of 1/PAR(1)).
C
      DPDSA = 1.0/SQRT(1.0-SINA**2)
      DSDT1 = -(SINA + D0/RAD)/(R0 - D0)
      DSDT1 = -DSDT1*R0**2
      DSDT4 = SINA/(R0 - D0) - 1.0/RAD
C
      DPDT1 = DPDSA*DSDT1
      DPDT3 = 1.0
      DPDT4 = DPDSA*DSDT4
C Calculate error on PHIT due to errors on TPC track (also include some
C protection, incase COV was not positive definate).
      ERPHI = DPDT1*(COV(1)*DPDT1 + 2.0*COV(4)*DPDT3 + 2.0*COV(7)*DPDT4)
     +      + DPDT3*(COV(6)*DPDT3 + 2.0*COV(9)*DPDT4)
     +      + DPDT4*COV(10)*DPDT4
      ERPHI = SQRT(ABS(ERPHI))
C The next line displays a lack of faith in the TPC covariance matrix.
      ERPHI = MIN(ERPHI,0.020)
C
      SPSI2 = 0.5*FACT2/R0
      IF (SPSI2.LT.0.0.OR.SPSI2.GT.1.0) GOTO 999
      PSI = ASIN(SQRT(SPSI2))
      XPAR(2) = PAR(5) + 2.0*PSI*ABS(R0)*PAR(2)
      IF(ABS(XPAR(2)).GT.WZMXIT) GOTO 999
C
      SINB = (0.5*FACT1+R0*D0)/(RAD*R0)
      IF(ABS(SINB).GT.1.0) GOTO 999
      XPAR(3) = ASIN(SINB)
C Make approx. correction to XPAR(1) for the fact that the fact that
C the cylinder which the track intersects has an alignment offset.
C Don't bother with XPAR(2) as z-resolution is so bad.
      RT = RAD
      ZT = XPAR(2)
      CALL ITROTN(RT,PHIT,ZT)
      EXTPH = XPAR(1) - PHIT
      DELPH = (1.0-RAD/RT)*TAN(XPAR(3))
      XPAR(1) = XPAR(1) + DELPH
      XPAR(1) = MOD(XPAR(1)+TWOPI,TWOPI)
C Find the distance between the intercept of the track with the
C scattering radius and that with this layer of the ITC.
      DIST = SQRT(RSCAIT**2 + RAD**2 -
     +         2.0*RSCAIT*RAD*COS(TPHIS-XPAR(1)))
C
C Find the 1 sigma uncertainties on XPAR(1) and XPAR(2).
C Protect against square root of negative value... (P.Comas)
      ROAD(1) = SQRT(MAX(0.0, 1.0-(0.5*DIST/R0)**2)
     +        *(THSCT*DIST/COS(XPAR(3)))**2
     +        + (RAD*ERPHI)**2)
C Set ROAD(2) = 0.0 as it is negligable compared with ITC resolution.
      ROAD(2) = 0.0
C
C Find wire numbers to be searched.
C First note the range in phi to be searched (include alignment
C correction).
      RD1 = SQRT(ROAD(1)**2 + MIN(2.0*SIGRIR(LAYER),SMAXIR(LAYER))**2)
      PHID = XPAR(1) + EXTPH - SIGNUM(ITRY)*RD1/RAD
      PHIU = XPAR(1) + EXTPH + SIGNUM(ITRY)*RD1/RAD
      PHID = MOD(PHID+TWOPI,TWOPI)
      PHIU = MOD(PHIU+TWOPI,TWOPI)
C Note cell size and phi shift beginning of 1st cell.
      DLN  = TWOPI/FLOAT(NWIRIT(LAYER))
      SHFN = PHWRIT(LAYER) - DLN*0.5
C
      PHDS = PHID - SHFN
      PHUS = PHIU - SHFN
      PHDS = AMOD(PHDS+TWOPI,TWOPI)
      PHUS = AMOD(PHUS+TWOPI,TWOPI)
      IWWIN(1) = INT(PHDS/DLN) + IWIRIT(LAYER) + 1
      IWWIN(2) = INT(PHUS/DLN) + IWIRIT(LAYER) + 1
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.6) THEN
        WRITE(LDEBRL,5001) PAR(1),LAYER,XPAR(1),ROAD(1),XPAR(2),ROAD(2),
     +  IWWIN(1),IWWIN(2)
 5001   FORMAT(' INSECT : Radius=',E10.3,' LAYER=',I1,' Phi intersect=',
     +  F6.3,'+-',F6.3,' Z intersect=',F6.1,'+-',F6.1,
     +  ' Wire search=',I3,'->',I3)
      ENDIF
C
      IER = 0
C
  999 CONTINUE
      END
#endif
