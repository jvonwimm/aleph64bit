      SUBROUTINE UMSERR (IR,NITC,NVDET,HP,COVIN,
     +                   COVOU,IERR)
C --------------------------------------------------------------
C! Add multiple scattering contribution to track error matrix
CKEY COMPUTE MULTIPLE SCATTERING /USER
C
C NOTE: 1991 VDET and beampipe geometry not yet in
C
C     Author: R. Johnson  (adapted from UINCOV by LL. Garrido)
C             22-01-91
C
C     Mods: S. Wasserbaech, 6 May 1991.
C           Adjusted material amounts, including VDET supports
C           and carbon fiber tube.
C           Corrected tan(lambda) dependence of multiple
C           scattering errors.
C           Improved the treatment of the off-diagonal errors.
C           R. Jacobsen Sept. 1991
C           Added regions 4 and 5 (see IR below)
C           Adjusted material to handle 1991 Vdet config
C           F.Ranjard, 3 June 1992
C           call ABRUEV to get current run number
C           call GTSTUP at every new run to get VDET setup code.
C           declare GTSTUP INTEGER  variable.
C
C     Input::
C          IR: =1 Means you want the error matrix to be valid
C                 in the region of the interaction point
C              =2 Means you want the errors in the ITC region
C              =3 Means you want the errors in the TPC region
C              =4 Means you want the errors to include all material
C                 outside and including Vdet outer layer, but not
C                 the Vdet inner layer and beampipe
C              =5 Means you want the errors to include all material
C                 in the Vdet (i.e. both layers) but not the beampipe
C
C          NITC:  Number of points in the ITC used in fitting this
C                 track (can be obtained in the FRTL bank)
C          NVDET: Number of points in the VDET used in fitting this
C                 track (can be obtained in the FRTL bank)
C          HP:    The 5 parameters of the helix as in FRFT
C          COVIN: Covariance matrix in triangular form as in FRFT
C    Output::
C          COVOUT: Output covariance matrix in  triangular form
C                   (15 elements)
C          IERR: Error flag,
C                0= ok
C                3= IR out of range
C ---------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "alcons.h"
#include "evehjj.h"
#include "bcs.h"
C
      REAL    ALFIEL
      INTEGER GTSTUP, NAMIND
C
      DIMENSION HP(*),COVIN(*),COVOU(*)
C
      PARAMETER (MXSCT=5,MXREG=5,MXDET=3)
      DIMENSION RMS(MXSCT),XLEN(MXSCT),FUSE(MXSCT,MXREG),
     &          FSCT(MXSCT,MXDET),S(2),PHI(2),Z(2),
     &          NPRG(MXSCT),PHMN(MXREG,MXSCT),PHMX(MXREG,MXSCT)
      LOGICAL FUSE,FSCT
C
      DATA SNMX/0.99/    !Maximum sin(angle track and radius vector)
      DATA ALPD/.0141/   !Constant from PDG for multiple scattering
C
C++   Geometry constants (need mods to get these from data base)
C++   The scatterers are
C++       (1) beampipe + carbon fiber tube + VDET support
C++       (2) VDET inner layer
C++       (3) VDET outer layer
C++       (4) VDET support + ITC inner wall
C++       (5) ITC wires + gas + outer wall + TPC inner field cage
C
      DATA NSCAT/5/
      DATA RMS / 8.2,    9.6,    11.4,   12.5,   30.0   /
      DATA XLEN/ 0.0100, 0.0137, 0.0137, 0.0031, 0.0316 /
      DATA FUSE/.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,
     &          .FALSE.,.FALSE.,.FALSE.,.FALSE.,.TRUE.,
     &          .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &          .FALSE.,.FALSE.,.TRUE.,.TRUE.,.TRUE.,
     &          .FALSE.,.TRUE.,.TRUE.,.TRUE.,.TRUE./
      DATA FSCT/.TRUE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &          .TRUE.,.TRUE.,.TRUE.,.TRUE.,.FALSE.,
     &          .TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE./
      DATA NPRG/1,1,1,1,1/
      DATA PHMN/25*0./
      DATA PHMX/25*6.28318532/
      DATA KOLD /0/
C
      IERR = 0
C
C++   Copy the existing error matrix to the output
C
      CALL UCOPY(COVIN,COVOU,15)
C
      IF (IR.LT.1 .OR. IR.GT.5) THEN
        IERR=3
        RETURN
      ENDIF
C
C++   Find the current run number to get the VDET geometry type
C
      CALL ABRUEV (KRUN,KEVE)
      IF (KRUN.NE.KOLD) THEN
         KOLD = KRUN
         IVS  = GTSTUP ('VD',KRUN)
      ENDIF
C
C++   Get the VDET geometry constants appropriate to this run
C
      IF (IVS .EQ. 1) THEN
C
C  1989; 2 litle bits
C
        NPRG(2)=1
        PHMN(1,2)=4.189
        PHMX(1,2)=5.236
        NPRG(3)=1
        PHMN(1,3)=4.084
        PHMX(1,3)=5.341
      ELSE IF(IVS .EQ. 2)THEN
C
C  1990; Full inner layer, parts of the outer
C
        NPRG(2)=1
        PHMN(1,2)=0.
        PHMX(1,2)=TWOPI
        NPRG(3)=2
        PHMN(1,3)=0.733
        PHMX(1,3)=1.990
        PHMN(2,3)=3.665
        PHMX(2,3)=5.341
      ELSE IF(IVS .GE. 3)THEN
C
C  1991; Full VDET
C
        RMS(1) = 5.355
        RMS(2) = 6.293
        RMS(3) = 10.98
      ENDIF
C
C++   If the user wants errors in the TPC region, then there is
C++   nothing to do (although the errors won't be correct if the
C++   track has ITC points).
C
      IF (IR.EQ.3) RETURN
C
C++   What is the innermost region included in the track fit?
C
      IF (NVDET.GT.0) THEN
        IIN=1
      ELSEIF (NITC.GT.0) THEN
        IIN=2
      ELSE
        IIN=3
      ENDIF
C
C++   Get the magnetic field strength and the momentum
C
      BFIEL=ALFIEL(DUM)
      RK= HP(1)
      PT=ABS(BFIEL*CLGHT/(RK*100000.))
      TL= HP(2)
      CSL2=1.+TL**2
      CSL=SQRT(CSL2)
      PM=PT*CSL
      PHI0= HP(3)
      D0= HP(4)
      RAD=1./ABS(RK)
C
C++   Loop over the scattering radii.
C
      DO 1 I=1,NSCAT
C
C++     See if UMSERR was requested to use this scattering radius.
C
        IF (.NOT.FUSE(I,IR)) GO TO 1
C
C++     Don't include scattering at a radius outside that of the
C++     first coordinate used in the track fit
C
        IF (.NOT.FSCT(I,IIN)) GO TO 1
C
C++     Check if the helix is always outside the scattering radius
C
        IF (ABS(D0).GT.RMS(I)) GO TO 1
C
C++     Check if the helix is always inside the scattering radius
C++     (This should never happen, since we already checked that
C++     the radius is inside the first coordinate used in the fit)
C
        IF (ABS(2./RK-D0).LT.RMS(I)) GO TO 1
C
C++     Calculate the intersection of the track with this layer
C
        CALL THLCIR(HP,RMS(I),S,PHI,Z,IER)
        IF (IER.NE.0) GO TO 1
C
C++     Is phi within range of the detector material?
C
        DO 32 K=1,NPRG(I)
          IF (PHI(1).GE.PHMN(K,I) .AND. PHI(1).LE.PHMX(K,I)) THEN
            GO TO 33
          ENDIF
   32   CONTINUE
        GO TO 1
   33   CONTINUE
C
C++     Calculate the angle between the track and the radius vector
C++     (in the r-phi projection) at the scattering radius.
C
        SINB = (0.5*RK*(RMS(I)-D0)*(RMS(I)+D0)+D0)/RMS(I)
C
C++     Skip this layer if the track is just grazing the material
C
        IF (ABS(SINB).GT.SNMX) GO TO 1
        CBETA = SQRT((1.-SINB)*(1.+SINB))
C
C++     Calculate the rms projected scattering angle
C
        XLNT= XLEN(I)*CSL/CBETA
        ALFA= (ALPD/PM)*SQRT(XLNT)
C
C++     Calculate the turning angle of the helix from the origin
C++     to the scattering point.  S(1)=arc length to scattering point,
C++     so S(1)/CSL= arc length in the projected plane (x,y)
C
        TMS= S(1)/(RAD*CSL)
        SENMS=SIN(TMS)
        COSMS=COS(TMS)
C
C++     Accumulate in quadrature the contributions to errors on
C++     the helix parameters.  These equations are overkill.  To
C++     a good approximation, X1=ALFA and X2= (S(1)/CSL)*ALFA.
C
        X1 = ALFA * CSL * COSMS / (1.-D0*RK)
        X2 = ALFA * CSL * SENMS * RAD
        Z1 = ALFA * CSL2
        Z2 = -ALFA * CSL * S(1)
C
C++   Add multiple scattering in quadrature with the existing
C++   diagonal terms of the covariance matrix
C
      COVOU( 3) = COVOU( 3) + Z1**2
      COVOU( 6) = COVOU( 6) + X1**2
      COVOU(10) = COVOU(10) + X2**2
      COVOU(15) = COVOU(15) + Z2**2
C
C++   Off-diagonal terms:
C
      COVOU( 9) = COVOU( 9) + X1*X2
      COVOU(12) = COVOU(12) + Z1*Z2
    1 CONTINUE
C
C++   Now make sure the covariance matrix still
C++   makes sense:
C
      COVMAX = SQRT(AMAX1(0.,COVOU( 6)*COVOU(10)))
      IF (ABS(COVOU( 9)) .GT. COVMAX)
     &            COVOU( 9) = 0.999 * SIGN(COVMAX,COVOU( 9))
C
      COVMAX = SQRT(AMAX1(0.,COVOU( 3)*COVOU(15)))
      IF (ABS(COVOU(12)) .GT. COVMAX)
     &            COVOU(12) = 0.999 * SIGN(COVMAX,COVOU(12))
C
      END
#endif
