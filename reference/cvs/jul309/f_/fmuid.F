      SUBROUTINE FMUID(ITK,PRPI,PRK,PRMU,IFLAG)
C----------------------------------------------------------------------
C! Find probability density function for pi,k,mu using mu-chamber.
C!
C!   Author   :- John Hilgart          13-FEB-1989
C!
C!   Inputs: ITK track no. in FRFT
C!
C!            PDF :== "probability density function"
C!   Outputs: PRPI  PDF for pi hypothesis
C!            PRK   PDF for kaon hypothesis
C!            PRMU  PDF for mu hypothesis
C!            IFLAG = 0 if mu chamber information was available for track
C!
C!   Description The algorithm will be described in tedious detail in
C!   =========== a future ALEPH note by Hilgart and Simopoulou.
C A1PI,A2PI, etc. give the 4 parameters for the pion probability dens
C function in the mu chambers, similarly,
C A1K,A2K, etc. for the kaons
C The XLAYR array has distance ratio cutoffs used in JULIA
C PUNCH gives punch-through probabilites for pi's, k's, and mu's measured
C    at a few energies
C XASS gives the relative probability of associating to 1 or 2 mu layers
C  for the different particles at the different energies.
C PINT delineates the momentum intervals appropriate to the PUNCH array.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "alcons.h"
#include "afidjj.h"
#include "frftjj.h"
#include "mcadjj.h"
      PARAMETER (LPINT = 4, LPI = 1, LK = 2, LMU=3)
      PARAMETER (LSPEC = 3)
      DIMENSION PUNCH(LSPEC,LPINT),
     &   PINT(LPINT),XLAYR(2),XASS(LSPEC,LPINT,2)
      DIMENSION RATI(2),A1K2(LPINT),A2K2(LPINT),
     &      A3K2(LPINT),A4K2(LPINT),A1PI(LPINT),A2PI(LPINT),A3PI(LPINT),
     &      A4PI(LPINT)
C
C These constants are very specific: They are coefficients for a fit.
C perhaps they will end up on the data base
      DATA A1PI /5.599E-2,4.748E-2,2.869E-2,2.869E-2/
      DATA A2PI /-1.660E-3,-2.322E-2,1.498E-2,1.498E-2/
      DATA A3PI /1.048,1.229,1.501,1.501/
      DATA A4PI /1.396,1.329,.9852,.9852/
C
      DATA A1K2 /2.078E-2,.1899,.1789,.2935/
      DATA A2K2 /7.517E-3,-3.103E-2,-1.742E-2,-9.204E-2/
      DATA A3K2 /.6404,.5792,.6371,.7674/
      DATA A4K2 /2.525,1.831,1.466,1.169/
      DATA A1K1,A2K1,A3K1,A4K1 /4.81E-2,.5337,-.3653,8.599E-2/
C
      DATA  PUNCH /.0086,.0232,.967, .0090,.02405,.973,
     &             .00520,.02165,.961, .00785,.01885,.955/
      DATA PINT / 4.25,7.5,17.5,40./
C
      DATA XASS /.145,.233,.0900, .106,.210,.0843, .202,.259,.101,
     &   .476,.405,.0901,
     &   .855,.767,.910, .894,.790,.9157, .798,.741,.899,
     &   .424,.595,.9099/
C
      DATA IONC /0/
C
C Statement functions
#include "bmacro.h"
      FMU(RAT,IASS,ICHO) = XASS(LMU,ICHO,IASS)*2.*RAT*EXP(-RAT*RAT)
      FNOMU(A1,A2,A3,A4,RAT) = A1 + A2*RAT +
     & A3*RAT*EXP(-RAT*RAT/A4)
      FPI(RAT,IASS,ICHO) = XASS(LPI,ICHO,IASS)*
     &   FNOMU(A1PI(ICHO),A2PI(ICHO),A3PI(ICHO),A4PI(ICHO),RAT)
      FK1(RAT,ICHO) = XASS(LK,ICHO,1)*(A1K1 + RAT*(A2K1 +
     &   RAT*(A3K1 + RAT*A4K1)))
      FK2(RAT,ICHO) = XASS(LK,ICHO,2)*
     &   FNOMU(A1K2(ICHO),A2K2(ICHO),A3K2(ICHO),A4K2(ICHO),RAT)
C
      IF (IONC.EQ.0) THEN
C
         NMCAD = NAMIND('MCAD')
         NFRFT = NAMIND('FRFT')
C Get distance ratio cutoffs
         NMCUT = NAMIND('MCUT')
         JMCUT = IW(NMCUT)
         DO 1 ID = 1, 2
            XLAYR(ID) = 4.0
            IF (JMCUT.GT.0)  XLAYR(ID) = RW(JMCUT+2+ID)
    1    CONTINUE
C
C Magnetic field
         NAFID = NAMIND('AFID')
         KAFID = IW(NAFID)
         IF (KAFID.GT.0) THEN
            BFIEL = RTABL(KAFID,1,JAFIMF)
         ELSE
            BFIEL = 15.
         ENDIF
         BFLCLT = BFIEL*CLGHT*1.E-5
         IONC = 1
      ENDIF
      IFLAG = 1
      PRPI = 0.
      PRK = 0.
      PRMU = 0.
C
C simple tests for conditions under which I will not try to find a muon:
C Is there an MCAD bank?
      KMCAD = IW(NMCAD)
      KFRFT = IW(NFRFT)
      IF (KMCAD.LE.0)  GOTO 900
      DO 2 IROW = 1, LROWS(KMCAD)
         JMCAD = KROW(KMCAD,IROW)
         IF (IW(JMCAD+JMCATN).EQ.ITK)  GOTO 3
  2   CONTINUE
  3   IF (IROW.GT.LROWS(KMCAD))  GOTO 900
C Use inverse radius of curvature and tanL of the track from FRFT
      JFRFT = KROW(KFRFT,ITK)
      PT = ABS(BFLCLT/RW(JFRFT+JFRFIR))
      PZ = PT*RW(JFRFT+JFRFTL)
      PMOM = SQRT(PT**2 + PZ**2)
C
C Only consider TPC tracks w/ P>3Gev
      IF (ABS(PZ/PMOM).GT. .95 .OR. PMOM.LT.3.)  GOTO 900
C
      DO 10 ICHO = 1, LPINT
 10      IF (PMOM .LT. PINT(ICHO)) GOTO 11
 11   CONTINUE
      IF (ICHO.GT.LPINT)  ICHO = LPINT
C
C Get ratio from mu ass. bank
      IASS = 0
      DO 30 ID = 1, 2
         RATI(ID) = 999.
C We use the distance ratios  from the mu chambers
         CUTOF = RW(JMCAD+JMCADC+ID-1)
         DIST = RW(JMCAD+JMCADH+ID-1)
C
C Distance ratios
         IF (CUTOF .GT. 1.E-5 .AND. DIST .GT. 1.E-5)  RATI(ID) =
     &      XLAYR(ID)*DIST/CUTOF
         IF (RATI(ID) .LT. 3.)  IASS = IASS + 1
 30   CONTINUE
      IF (IASS.EQ.0)  GOTO 900
      RAT = RATI(1)
      IF (RAT .GT. 3.) RAT = RATI(2)
C
C We now calculate the probability density functions for mu,pi,k
      PRMU = PUNCH(LMU,ICHO)*FMU(RAT,IASS,ICHO)
C
C The k's and pi's behave differently if only 1 layer is hit. mu's do not.
      IF (IASS .EQ. 1) THEN
C
C W/ pi's we don't really know the distribution very accurately.
         PRPI = PUNCH(LPI,ICHO)*FPI(RAT,IASS,1)
         PRK = PUNCH(LK,ICHO)*FK1(RAT,ICHO)
      ELSE
C
C The more common case of 2 associated hits
         PRPI = PUNCH(LPI,ICHO)*FPI(RAT,IASS,ICHO)
         PRK = PUNCH(LK,ICHO)*FK2(RAT,ICHO)
      ENDIF
      IFLAG = 0
      RETURN
C
  900 CONTINUE
      END
#endif
