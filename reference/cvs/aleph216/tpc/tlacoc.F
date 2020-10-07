      SUBROUTINE TLACOC( R,PHI,Z , RP,PHIP,ZP , MODE)
C-----------------------------------------------------------------------
C! Corrections for B-field distortions (phi independent)
C ====================================
CKEY   TPC LASER B-FIELD DISTORTIONS
C
C Author : Michael Schmelling   / Mainz   15-Jan-1990
C Modified : Werner Wiedenmann / 12-Sep-1991
C            (rename subroutine and call it from driver TLACOR)
C
C Input:    : real*4    R        : TPC coordinate - radius  [cm]
C             real*4    PHI      : TPC coordinate - azimuth [rad]
C             real*4    Z        : TPC coordinate - Z       [cm]
C             integer*4 MODE     : radial correction mode
C
C                       MODE.EQ.0 => corrections based exclusively
C                                    on laser data
C                       MODE.NE.0 => for full compensation use radial
C                                    corrections based on the field map
C             bank TLCP from data base
C
C
C Output:     real*4  RP,PHIP,ZP : corrected coordinates
C
C Common blocks       : BCS
C External references : AGETDB,JUNIDB,AOPDBS,TFCINI,TCOREC /ALEPHLIB
C
C-----------------------------------------------------------------------
C DESCRIPTION
C ===========
C TLACOC corrects individual TPC coordinates for distortions of the
C drift field, for the two cases of compensating coils power supplies
C on or off. The default corrections (MODE=0) were obtained from the
C analysis of YAG laser data. For the case of full compensation
C MODE=1 allows to use alternatively the radial corrections based on
C the B-field map from the survey measurements, which are interfaced
C through the old routine TCOREC.
C
C-----------------------------------------------------------------------
C MORE DETAILED INFORMATION
C =========================
C   The corrections were obtained from the analyis of YAG-laser data
C taken with and without magnetic field. Wire by wire the drift time
C differences between both field configurations were measured and
C corrected for changes in the steering of the laser beams and the
C drift velocity. Finally the drift time differences were translated
C into radial coordinate displacements which were parametrized for
C each beam by fourth or fifth order polynomials. Those parametri-
C zations exhibit a slight phi-dependence which might be introduced
C by a small misalignment between electric and magnetic field.
C Since it is roughly an order of magnitude smaller than the phi-
C independent part, it has been neglected - i.e. only the zero-order
C Fourier component has been retained. (As it turns out, the
C quality of the correction procedure is the same, independent of
C whether or not the phi-dependence is taken into account.)
C
C   For phi-symmetric field inhomogenieties the radial displacements
C mapped by the procedure decribed above are exactly omega*tau times
C the azimuthal coordinate displacements in the combined electric and
C magnetic field, even in the presence of electric field distortions.
C (Gigi came across this interesting fact.) The azimuthal corrections
C which are relevant for the momentum resolution of the TPC thus can
C be deduced very reliably.
C
C   The radial distortions of the combined E- and B-field can only be
C determined by the above method, if the the electric field is perfect,
C i.e., if without B-field the laser tracks are straight. For side B
C of the TPC this holds much better than for side A, where significant
C curvatures in r-z are observed close to the inner field cage. Since
C those track distortions are only observed in the absence of the
C magnetic field, they are likely to be of electrical origin. Radial
C coordinate displacements affect mainly the measurement of dip-angle
C and z0 of tracks. Since at full magnetic field electric distortions
C are suppressed by a factor omega*tau (9.) only the magnetic field
C inhomogenieties survive and have to be corrected for. By construction
C of the ALEPH coil the purely magnetic contribution to the radial
C coordinate shifts can be expected to be roughly symmetrical for both
C sides of the TPC. Because it can be measured more reliably on side B
C the radial corrections found there are applied in both halves.
C This in fact prooves to produce as function of the dip angle a flat
C <z0>-distribution for two track events.
C
C   The correction procedure has been tested with two track events
C taken with and without compensation of the magnetic field. Results:
C
C -  momentum resolution and scale become the same for postives and
C    negatives and independent of dip angle.
C
C -  <z0> becomes independent of dip angles
C
C -  <d0> becomes compatible with 0 for tan(lamda) < 1.2
C    for tan(lambda) > 1.2 an anomaly remains <d0> = 0.06 cm which
C    is not understood.
C
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "evehjj.h"
#include "ralejj.h"
#include "tlcpjj.h"
C
      LOGICAL FIRST, DONE
      INTEGER AGETDB
      DOUBLE PRECISION  X1,X2,Y1A,Y1B,Y2A,Y2B,DZ
C
      PARAMETER    (ZMAX=220., RMAX=180., OMEGAT=9.)
      PARAMETER    (CT30=1.732, CT39=1.235, CT67=0.425, R30=127.02)
C
      DATA FIRST/.TRUE./
      DATA DONE/.FALSE./
      DATA MXPRT/2/
C
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C      initialization
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        NARALE= NAMIND('RALE')
        NAEVEH= NAMIND('EVEH')
        NATLCP= NAMIND('TLCP')
        IOFF  = 0
        LRUN  = 0
      ENDIF
C
      RP   = R
      PHIP = PHI
      ZP   = Z
C
      IF(R.LE.0.) THEN
         CALL ALTELL(
     &     ' TLACOC: TPC coordinate with radius .le. 0  => RETURN',
     &     0,'RETURN')
         RETURN
      ENDIF
C
C      check that we have the right constants for the current run
C
      KEVEH=IW(NAEVEH)
      IF(KEVEH.EQ.0) THEN
         CALL ALTELL(' TLACOC: event header bank missing; no run #',
     &              0,'RETURN')
         RETURN
      ENDIF
      KRUN=IW(KEVEH+JEVERN)
C
C     Don't make any correction for Monte Carlo
C
      IF (KRUN.LT.2001) THEN
         RETURN
      ENDIF
C
      IF(KRUN.NE.LRUN) THEN
         NPRT=0
         LRUN=KRUN
C
C         get coefficients for this run
         IIII =  AGETDB('TLCP',KRUN)
         IF(IIII.EQ.0) THEN
           CALL ALTELL(
     &     ' TLACOC: Bank TLCP missing, cannot correct TPC coordinates',
     &     0,'RETURN')
           RETURN
         ENDIF
         KTLCP = IW(NATLCP )
C
C          bank  TLCP has 6 columns ( theta= 30,39,67 deg side A
C                                            30,39,67 deg side B )
C                    and NCOEF*2 rows.
C                    the first  NCOEF rows correspond to trim coils off
C                    the second NCOEF rows to trim coils on.
C
         NCOEF=LROWS(KTLCP)/2
C
C          were trim coils powered ?
C          if bank RALE not present,  assume they were powered: IOFF>0
         IOFF = NCOEF
         IF(IW(NARALE).NE.0) THEN
           IF(ITABL(IW(NARALE),1,JRALMA).LT.10000.OR.
     &        ITABL(IW(NARALE),1,JRALMB).LT.10000    ) IOFF=0
         ENDIF
C
C           if requested read displacement table based on the field map
         IF(MODE.NE.0.AND.IOFF.GT.0) THEN
C            IF(.NOT.DONE) THEN
C                CALL AOPDBS(' ',IER)
C                DONE   = .TRUE.
C            ENDIF
            CALL TFCINI(IW(6),JUNIDB(0),KRUN,IER)
         ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      KTLCP = IW(NATLCP)
      IF (KTLCP.EQ.0) THEN
        IF (NPRT.LT.MXPRT) THEN
          NPRT=NPRT+1
          CALL ALTELL(
     &    ' TLACOC: Bank TLCP missing, cannot correct TPC coordinates',
     &    0,'RETURN')
        ENDIF
      ENDIF
C
      RR = R/RMAX
      DZ = ZMAX - ABS(Z)
C
C  This is the Forward Region ....
      IF((ABS(Z)/R).GT.CT39 .AND. R.LT.R30) THEN
         X1  = ZMAX - R*CT30
         X2  = ZMAX - R*CT39
         IRGN= 3
C
C This is the Central Region ....
      ELSE
         X1  = ZMAX - R*CT39
         X2  = ZMAX - R*CT67
         IRGN= 4
      ENDIF
C
C Compute Polynomials...
      Y1A = RTABL(KTLCP,NCOEF+IOFF,IRGN+1)
      Y2A = RTABL(KTLCP,NCOEF+IOFF,IRGN+2)
      Y1B = RTABL(KTLCP,NCOEF+IOFF,IRGN+4)
      Y2B = RTABL(KTLCP,NCOEF+IOFF,IRGN+5)
      DO 10 JCOEF=NCOEF-1,1,-1
         Y1A = RTABL(KTLCP,JCOEF+IOFF,IRGN+1) + RR*Y1A
         Y2A = RTABL(KTLCP,JCOEF+IOFF,IRGN+2) + RR*Y2A
         Y1B = RTABL(KTLCP,JCOEF+IOFF,IRGN+4) + RR*Y1B
         Y2B = RTABL(KTLCP,JCOEF+IOFF,IRGN+5) + RR*Y2B
 10   CONTINUE
C
      S   = 1. - (Z/ZMAX)**2
      DRA = (Y1A + (DZ-X1)*(Y2A-Y1A)/(X2-X1)) * S
      DRB = (Y1B + (DZ-X1)*(Y2B-Y1B)/(X2-X1)) * S
C
C  r-z correction according to MODE
      IF(MODE.EQ.0.OR.IOFF.EQ.0) THEN
         RP = R  - DRB
      ELSE
         CALL TCOREC(RP,PHIP,ZP)
      ENDIF
C
C  phi correction
      IF(Z.GT.0.) THEN
         PHIP = PHI + DRA/R/OMEGAT
      ELSE
         PHIP = PHI + DRB/R/OMEGAT
      ENDIF
C
      RETURN
      END
#endif
