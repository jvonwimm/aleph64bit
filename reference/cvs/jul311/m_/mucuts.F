        SUBROUTINE MUCUTS(ITRAC,C0,C1,C2)
C----------------------------------------------------------
C!    Coeffic for muon assoc.dist.and scatt.angle.
C!
C!    G.Capon                   9-Oct-87
C!
C!       Input   : ITRAC = track #
C!       Output _ :  coefficients C0,C1,C2 used in routine MUTEST
C!                   to compute radial and angular acceptance cuts
C!                  (according to mult. scatt.) for muon hits.
C!                    C2 = <Theta-projected **2> at Hcal exit
C!
C!   local variables :  RADL (IG)  =  Radiation length
C!         IG=1,2,3 for Ecal, Coil, Hcal average medium
C
C!
C!
C?
C!======================================================================
#ifndef DOC
      PARAMETER(NSTEP=26)
      DIMENSION RADL(3)
      DIMENSION SCOO(NSTEP+4),STEP(NSTEP+4),TH2PR(NSTEP+4)
      PARAMETER (WMU2=0.011164)
#include "bcs.h"
#include "trexjj.h"
#include "alcons.h"
      DATA RADL /2.4,8.05,2.45/
#include "bmacro.h"
C
C               preset values
C
      CALL VZERO (TH2PR,NSTEP+4)
      SCOO(1)=0.
      C0=0.
      C1=0.
      C2=0.
      JTREX=NLINK('TREX',ITRAC)
      NVETT=LROWS(JTREX)
C
C          compute partial lengths of trajectory and partial m.s. angle
C
      DO 10 N=1,NVETT-1
      DX=RTABL(JTREX,N+1,JTREX3)-RTABL(JTREX,N,JTREX3)
      DY=RTABL(JTREX,N+1,JTREX3+1)-RTABL(JTREX,N,JTREX3+1)
      DZ=RTABL(JTREX,N+1,JTREX3+2)-RTABL(JTREX,N,JTREX3+2)
      STEP(N)=SQRT(DX*DX+DY*DY+DZ*DZ)
      SCOO(N+1)=SCOO(N)+STEP(N)
      NMED=ITABL(JTREX,N,JTRERG)/1000
      IF(NMED.EQ.0) GO TO 10
      PX=RTABL(JTREX,N,JTREP3)
      PY=RTABL(JTREX,N,JTREP3+1)
      PZ=RTABL(JTREX,N,JTREP3+2)
      PTOT2=PX*PX+PY*PY+PZ*PZ
      ETOT2=PTOT2+WMU2
      PIBE2=PTOT2*(PTOT2/ETOT2)
      CORR=1.
      CORR   =  (1.+ LOG10 (STEP(N)/RADL(NMED))/9.)**2
      TH2PR(N)=225.0E-6/PIBE2*STEP(N)/RADL(NMED)*CORR
 10   CONTINUE
C
C          compute coefficients C0,C1,C2 for multiple scattering cuts
C               loop on extrapol. trajectory points
C
      DO 30 N=1,NVETT-1
      DSN=SCOO(NVETT)-SCOO(N+1)
      C0=C0+TH2PR(N)*(STEP(N)*STEP(N)/3.+STEP(N)*DSN+DSN*DSN)
      C1=C1+TH2PR(N)*(STEP(N)+2.*DSN)
      C2=C2+TH2PR(N)
 30   CONTINUE
C
      RETURN
      END
#endif
