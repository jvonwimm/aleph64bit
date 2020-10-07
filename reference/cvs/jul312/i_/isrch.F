      SUBROUTINE ISRCH(ITRY,ITYP,IL,XPAR,ROAD,IWWIN,PAR,OUT,DSPIR)
C-----------------------------------------------------------------------
C!   Search ITC coordinates in specified road.
C!
C!    Author      :- J. Sedgbeer 87/02/09
C!    Modified    :- J. Sedgbeer 88/05/09
C!    Modified    :- I. Tomalin  89/09/01
C!    Modified    :- I. Tomalin  90/01/19
C!
C!   Input:
C!    ITRY  /I : Type of search flag
C!    ITYP  /I : Search type:
C!                         = 1 closest hit only.
C!                         = 2 all hits in road (max NMAX) Test r-phi
C!    IL     /I : ITC layer number
C!    XPARi  /R4: phi, z and angle of intersection
C!    ROADi  /R4: 1 sigma uncertainties on XPAR(1) and XPAR(2)
C!    IWWINi /I4: wire numbers bounding the road
C!    PARi   /R4: TPC track parameters.
C!                i=1,6 (sig*r0,tanl,phi0,d0,z0,TPC track no.)
C!    OUT    /L4  : .TRUE. if particle moved outwards though the
C!                     detector along the current arc.
C!    DSPIR  /R4  : Approximately, this is the path length which
C!                     the particle has traversed.
C!    need commons /BCS/ for ITCO and work banks
C!                 /ITWICC/ for ITC geometry
C!                 /IWBNKT/ for pointers to work banks
C!                 /ITRKPA/ for tracking constants
C!                 /ITRKER/ for tracking const.
C!         params.  ALCONS
C!                  ITCOJJ
C!
C!   Output:
C!      Fills common /IROADC/ with information about coordinates inside
C!      the road in layer IL.
C!
C!   called by : IGICHA
C!   calls real function IPHCOR for drift-time correction
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
#include "bcs.h"
#include "itwicc.h"
#include "itrkpa.h"
#include "itrker.h"
#include "itcojj.h"
#include "alcons.h"
#include "iwbnkt.h"
#include "iroadc.h"
C-----------------------------------------------------------------------
      INTEGER NLINK
      EXTERNAL NLINK
      LOGICAL OUT
      REAL IPHCOR
      EXTERNAL IPHCOR
      DIMENSION XPAR(*),ROAD(*),IWWIN(*),PAR(*)
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Get ITCO bank and number of coords.
C
      NC=0
      KITC0=NLINK('ITCO',0)
      KITC1=NLINK('ITCO',1)
      PHTRK=XPAR(1)
      ZTRK=XPAR(2)
      ENANG=XPAR(3)
C
C Correct the drift time for the fact that the track spiralled several
C times around the central detector, if necessary.
      R0 = PAR(1)
      D0 = PAR(4)
C First find the r-phi component of the drift time correction.
      TRCOR = DSPIR/CLGHT
      IF (.NOT.OUT) TRCOR = TRCOR + 2.0*ABS(R0)/CLGHT*
     +ASIN(1.0-(RWIRIT(IL)**2-D0**2)/(2.0*R0*(R0-D0)))
C Then find the z component of the drift time correction.
      TZCOR = TRCOR*ABS(PAR(2))
C Hence get the total correction.
      TCOR = SQRT(TRCOR**2+TZCOR**2)
C     WRITE(LDEBRL,5000) PAR(6),DSPIR,TCOR,OUT
 5000 FORMAT(' PART=',F3.0,' DSPIR=',E12.3,' DTCOR=',E12.3,' OUT=',L4)
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.5) THEN
        WRITE(LDEBRL,1000) IL,PHTRK,ZTRK,ENANG
 1000   FORMAT(' ISRCH: LAYER',I2,' PH-Z-ANG',3F9.3)
      ENDIF
C
      NFND  = 0
      APMIN = 999.
C
C Loop over the wires inside the window (with care at 0-2pi cross-over)
C
      IWD = IWWIN(1)
      IWU = IWWIN(2)
      IF (IWU.LT.IWD) IWU = IWU + NWIRIT(IL)
      DO 40 ILOOP=IWD,IWU
        IWIRE = ILOOP
        IF (IWIRE.GT.IWIRIT(IL)+NWIRIT(IL)) IWIRE = IWIRE - NWIRIT(IL)
C See if this wire has an unused hit associated with it.
        IC = IW(IWHTIW+LMHLEN+IWIRE)
        IF (IC.EQ.0) GOTO 40
        IUSED = IW(IUCOIW+LMHLEN+IC)
        IF (IUSED.NE.0) GOTO 40
C
        ZZ    = RTABL(KITC1,IC,JITCZH)
C Produce the corrected phi coords.
        PHI1  = IPHCOR(IC,-1,ENANG,-TCOR)
        PHI2  = IPHCOR(IC, 0,ENANG,-TCOR)
        SRPQ  = RTABL(KITC1,IC,JITCSR)
C Check that coord. is in cell
        DIST = 0.5*RWIRIT(IL)*(MOD(PHI1-PHI2+5.0*PI,TWOPI)-PI)
        IF (DIST.GT.0.5*CELWIT(IL)+SIGNUM(ITRY)*SQRT(SRPQ))
     +        GOTO 40
        SZ    = RTABL(KITC1,IC,JITCSZ)
C Get r-phi resolution, corrected for entrance angle (protected against
C singularity).
        SRP   = SRPQ/MAX(ABS(COS(ENANG)),0.1)**2
C Find difference in phi (being careful about 0-2pi boundary).
        P1    = MOD(PHTRK-PHI1+5.0*PI,TWOPI) - PI
        P2    = MOD(PHTRK-PHI2+5.0*PI,TWOPI) - PI
        A1    = ABS(P1)*RWIRIT(IL)
        A2    = ABS(P2)*RWIRIT(IL)
        AZ    = ABS(ZTRK-ZZ)
C
        IF(FDEBRF.AND.JDBDRF(JULIT).GE.6) THEN
          WRITE(LDEBRL,1001) IC,IWIRE,RWIRIT(IL),SZ,PHI1,PHI2,ZZ,
     +    A1,A2,AZ
 1001     FORMAT(' ISRCH: COORD',I4,' WIRE',I5,' RWIRIT ',F6.3,
     +    ' SZQ',F9.1,/15X,' PHI1-PHI2-ZZ',2F8.4,F7.2,'  A1-A2',2F9.3,
     +    /15X,'  AZ',F9.3)
        ENDIF
C
C Apply Z-road test if requested. (Check that z coord. is O.K.)
C
        IF (FZTSIT(1)) THEN
C          IF(SZ.GT.10000.)  GOTO 40
          IF(AZ.GT.ZFACIT(1)*SQRT(SZ + ROAD(2)**2)) GOTO 40
        ENDIF
C
C Apply r-phi road test. If ok add hit to list.
C
        IF(A1.LT.SIGNUM(ITRY)*SQRT(SRP + ROAD(1)**2)) THEN
          IF(ITYP.EQ.1) THEN
            IF(A1.GT.APMIN) GOTO 30
            APMIN = A1
            NFND  = 0
          ENDIF
          IF(NFND.LT.NLMXIR) THEN
            NFND = NFND + 1
            IHITIR(NFND,IL) = IC
C Put the corrected coordinate into ITCO bank #0
            RW(KROW(KITC0,IC)+JITCP1) = PHI1
            RW(KROW(KITC0,IC)+JITCSR) = SRP
          ENDIF
        ENDIF
C
C Now test ambiguity. Ignore it if PHI1 and PHI2 are the same.
C
        DPHI = MOD(PHI1-PHI2+5.0*PI,TWOPI) - PI
        IF (RWIRIT(IL)*ABS(DPHI).LE.0.01*SQRT(SRP)) GOTO 40
C
   30   IF(A2.LT.SIGNUM(ITRY)*SQRT(SRP + ROAD(1)**2)) THEN
          IF(ITYP.EQ.1) THEN
            IF(A2.GT.APMIN) GOTO 40
            APMIN = A2
            NFND  = 0
          ENDIF
          IF(NFND.LT.NLMXIR) THEN
            NFND = NFND + 1
            IHITIR(NFND,IL) = -IC
C Put the corrected coordinate into ITCO bank #0
            RW(KROW(KITC0,IC)+JITCP2) = PHI2
            RW(KROW(KITC0,IC)+JITCSR) = SRP
          ENDIF
        ENDIF
   40 CONTINUE
C
   50 NHITIR(IL) = NFND
C
  999 CONTINUE
      END
#endif
