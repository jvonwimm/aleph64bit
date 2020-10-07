      SUBROUTINE TLACOP( R,PHI,Z , RP,PHIP,ZP , MODE)
C-----------------------------------------------------------------------
C! Corrections for B-field distortions (phi dependent)
C ====================================
CKEY   TPC LASER B-FIELD DISTORTIONS
C
C Author : Michael Schmelling   / Mainz   15-Jan-1990
C Modif. : Werner Wiedenmann  17.7.1991
C          after the model of  TLACOR/TLACOC the subroutine was
C          modified to read the phi dependent laser corrections
C          from the TLFC bank.
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
C             bank TLFC from data base
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
C TLACOP corrects individual TPC coordinates for distortions of the
C drift field, for the two cases of compensating coils power supplies
C on or off. The default corrections (MODE=0) were obtained from the
C analysis of YAG laser data. For the case of full compensation
C MODE=1 allows to use alternatively the radial corrections based on
C the B-field map from the survey measurements, which are interfaced
C through the old routine TCOREC.
C
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "evehjj.h"
C
      LOGICAL FIRST, QEXTRA
      INTEGER AGETDB
      DOUBLE PRECISION  X1,X2,Y1A(3),Y1B(3),Y2A(3),Y2B(3),DZ
      DOUBLE PRECISION        P1A(3),P1B(3),D1A(3),D1B(3)
      DOUBLE PRECISION        YY1A  ,YY1B  ,YY2A  ,YY2B
C
      PARAMETER    (IPRE=3)
      PARAMETER    (IPHI=3)
      PARAMETER    (ZMAX=220., RMAX=180., OMEGAT=9.)
      PARAMETER    (CT18=3.078, CT30=1.732, CT39=1.235, CT67=0.425)
      PARAMETER    (R30=127.02, R18=71.47)
C
      DATA FIRST/.TRUE./
      DATA MXPRT/2/
C
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C      initialization
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        NAEVEH= NAMIND('EVEH')
        NATLFC= NAMIND('TLFC')
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
     &     ' TLACOP: TPC coordinate with radius .le. 0  => RETURN',
     &     0,'RETURN')
         RETURN
      ENDIF
C
C      check that we have the right constants for the current run
C
      KEVEH=IW(NAEVEH)
      IF(KEVEH.EQ.0) THEN
         CALL ALTELL(' TLACOP: event header bank missing; no run #',
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
         IIII =  AGETDB('TLFC',KRUN)
         IF(IIII.EQ.0) THEN
           CALL ALTELL(
     &     ' TLACOP: Bank TLFC missing, cannot correct TPC coordinates',
     &     0,'RETURN')
           RETURN
         ENDIF
         KTLFC = IW(NATLFC )
C
C          bank  TLFC has 3+8*3 columns ( theta= 18,30,39,67 deg side A
C          18,30,39,67 deg side B and 3 phi-planes for each theta)
C          and NCOEF rows.
C
         NCOEF=LROWS(KTLFC)
         NCOLS=(LCOLS(KTLFC)-IPRE)/2
         IOFF=0
C
C           if requested read displacement table based on the field map
         IF(MODE.NE.0) THEN
            CALL TFCINI(IW(6),JUNIDB(0),KRUN,IER)
         ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      KTLFC = IW(NATLFC)
      IF (KTLFC.EQ.0) THEN
        IF (NPRT.LT.MXPRT) THEN
          NPRT=NPRT+1
          CALL ALTELL(
     &    ' TLACOP: Bank TLFC missing, cannot correct TPC coordinates',
     &    0,'RETURN')
        ENDIF
      ENDIF
C
      RR = R/RMAX
      DZ = ZMAX - ABS(Z)
      CTEST = ABS(Z)/R
      SINP  = SIN(PHI)
      COSP  = COS(PHI)
      QEXTRA = .FALSE.
      REXTRA = 0.
C
C-----------------------------------------------------------
C++     Make phi dependent correction
C-----------------------------------------------------------
C
C  This is the very Forward Region ....
         IF(CTEST.GT.CT30) THEN
            X1  = ZMAX - R*CT18
            X2  = ZMAX - R*CT30
            IRGN= 1
            IF (R.GT.R18) THEN
               QEXTRA=.TRUE.
               REXTRA=R18/RMAX
            ENDIF
         ENDIF
C
C  This is the Forward Region ....
         IF(CTEST.GT.CT39 .AND. CTEST.LE.CT30) THEN
            X1  = ZMAX - R*CT30
            X2  = ZMAX - R*CT39
            IRGN= 2
            IF (R.GT.R30) THEN
               QEXTRA=.TRUE.
               REXTRA=R30/RMAX
            ENDIF
         ENDIF
C
C This is the Central Region ....
         IF(CTEST.LE.CT39) THEN
            X1  = ZMAX - R*CT39
            X2  = ZMAX - R*CT67
            IRGN= 3
         ENDIF
C
C-----------------------------------------------------------
C++     Compute Polynomials
C-----------------------------------------------------------
C
      DO 9 IB=1,IPHI
         Y1A(IB) = RTABL(KTLFC,NCOEF+IOFF,IPRE+(IRGN-1)*IPHI+IB)
         Y2A(IB) = RTABL(KTLFC,NCOEF+IOFF,IPRE+IRGN*IPHI    +IB)
         Y1B(IB) = RTABL(KTLFC,NCOEF+IOFF,IPRE+(IRGN-1)*IPHI+IB+NCOLS)
         Y2B(IB) = RTABL(KTLFC,NCOEF+IOFF,IPRE+IRGN*IPHI    +IB+NCOLS)
         DO 10 JCOEF=NCOEF-1,1,-1
            Y1A(IB)=RTABL(KTLFC,JCOEF+IOFF,IPRE+(IRGN-1)*IPHI+IB)
     >              + RR*Y1A(IB)
            Y2A(IB)=RTABL(KTLFC,JCOEF+IOFF,IPRE+IRGN*IPHI    +IB)
     >              + RR*Y2A(IB)
            Y1B(IB)=RTABL(KTLFC,JCOEF+IOFF,IPRE+(IRGN-1)*IPHI+IB+NCOLS)
     >              + RR*Y1B(IB)
            Y2B(IB)=RTABL(KTLFC,JCOEF+IOFF,IPRE+IRGN*IPHI    +IB+NCOLS)
     >              + RR*Y2B(IB)
 10      CONTINUE
C
C-----------------------------------------------------------
C++     Compute Extrapolation for Polynomials if necessary
C-----------------------------------------------------------
C
         IF (QEXTRA) THEN
            P1A(IB)=RTABL(KTLFC,NCOEF+IOFF,IPRE+(IRGN-1)*IPHI+IB)
            P1B(IB)=RTABL(KTLFC,NCOEF+IOFF,IPRE+(IRGN-1)*IPHI+IB+NCOLS)
            D1A(IB)=RTABL(KTLFC,NCOEF+IOFF,IPRE+(IRGN-1)*IPHI+IB)
     >             *FLOAT(NCOEF-1)
            D1B(IB)=RTABL(KTLFC,NCOEF+IOFF,IPRE+(IRGN-1)*IPHI+IB+NCOLS)
     >             *FLOAT(NCOEF-1)
            DO 11 JCOEF=NCOEF-1,1,-1
               P1A(IB)=REXTRA*P1A(IB) +
     >            RTABL(KTLFC,JCOEF+IOFF,IPRE+(IRGN-1)*IPHI+IB)
               P1B(IB)=REXTRA*P1B(IB) +
     >            RTABL(KTLFC,JCOEF+IOFF,IPRE+(IRGN-1)*IPHI+IB+NCOLS)
               IF (JCOEF.GE.2) THEN
                  D1A(IB) = REXTRA*D1A(IB) +
     >               RTABL(KTLFC,JCOEF+IOFF,IPRE+(IRGN-1)*IPHI+IB)
     >               *FLOAT(JCOEF-1)
                  D1B(IB) = REXTRA*D1B(IB) +
     >               RTABL(KTLFC,JCOEF+IOFF,IPRE+(IRGN-1)*IPHI+IB+NCOLS)
     >               *FLOAT(JCOEF-1)
               ENDIF
11          CONTINUE
            Y1A(IB) = D1A(IB)*(RR-REXTRA) + P1A(IB)
            Y1B(IB) = D1B(IB)*(RR-REXTRA) + P1B(IB)
         ENDIF
  9   CONTINUE
C
C-----------------------------------------------------------
C++     Compute Fourier series in phi
C-----------------------------------------------------------
C
      YY1A = Y1A(1) + SINP*Y1A(2) + COSP*Y1A(3)
      YY2A = Y2A(1) + SINP*Y2A(2) + COSP*Y2A(3)
      YY1B = Y1B(1) + SINP*Y1B(2) + COSP*Y1B(3)
      YY2B = Y2B(1) + SINP*Y2B(2) + COSP*Y2B(3)
C
C-----------------------------------------------------------
C++     Compute shift
C-----------------------------------------------------------
C
      S   = 1. - (Z/ZMAX)**2
      DRA = (YY1A + (DZ-X1)*(YY2A-YY1A)/(X2-X1)) * S
      DRB = (YY1B + (DZ-X1)*(YY2B-YY1B)/(X2-X1)) * S
C
C  r-z correction according to MODE
      IF(MODE.EQ.0) THEN
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
