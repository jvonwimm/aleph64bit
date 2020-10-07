      INTEGER FUNCTION UFECAL(X_ECAL,ERR_ECAL)
C!  Function to extrapolate the last track fit by UFTKAL to the ECAL
C!
C!          D. Casper 06/06/96
#ifndef DOC
      DIMENSION X_ECAL(*),ERR_ECAL(*)

#include "bcs.h"
#include "frftjj.h"

      DIMENSION HELIX(5), ERROR(5,5), EXIT(6), OUTER(7)
      PARAMETER (MPT = 40)
      INTEGER IUSED(MPT)
      DOUBLE PRECISION XSTS(5,MPT),XME(2,MPT),RF(MPT),VMEAS(2,2,MPT)

      DATA R1ECA,Z1ECA,R2ECA,Z2ECA,R1COI,R2COI
     +   / 186.2 , 254.2, 234., 297., 248.,264.7/
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)

      INTEGER UF2PNT,UF2XYZ,UFDALI
      INTEGER IRET

#include "bmacro.h"

      UFECAL = -1

C  Get KF info

      IRET = UFDALI(NLOW,NHIGH,XSTS,XME,RF,IUSED,VMEAS)

C  First step is to figure out if it hits barrel or endcap
C  Get track parameters at outermost point, in global coordinates

      FIELRC = ALFIEL(DUM)
      ICODE = UF2PNT(FIELRC,SNGL(RF(NLOW)),HELIX,ERROR)
      IF (ICODE.NE.0) RETURN

      OMEGA = HELIX(1)
      TANL = HELIX(2)
      PHI0 = HELIX(3)
      D0 = HELIX(4)
      Z0 = HELIX(5)

C  Gross extrapolation with AUHCYL

      RHO = 1./OMEGA
      PT=FIELRC*RHO*CLGHT/100000.
      THETA = ATAN2 (1.,TANL)
      STHET = SIN(THETA)
      PPINI  = ABS(PT/STHET)
      CPHI  = COS(PHI0)
      SPHI  = SIN(PHI0)
      CTHET = COS(THETA)
C
      Q = - SIGN(1.,OMEGA)
      OUTER(1) =+D0*SPHI
      OUTER(2) =-D0*CPHI
      OUTER(3) = Z0
      OUTER(4) = STHET*CPHI
      OUTER(5) = STHET*SPHI
      OUTER(6) = CTHET
      OUTER(7) = PPINI
      CALL AUHCYL(R1ECA,Z1ECA,FIELRC,Q,OUTER,EXIT,ICODE)
      IF(ICODE.EQ.0) RETURN

C  Extrapolate from last TPC coordinate to specified radius using
C  KF routines (we get the error matrix at the point this way)

      R_EXTR = SQRT(EXIT(1)**2 + EXIT(2)**2)

      IF (ABS(R_EXTR-R1ECA).LT.1.)THEN
        ICODE = UF2PNT(FIELRC,R1ECA,HELIX,ERROR)
      ELSE
        ICODE = UF2PNT(FIELRC,R_EXTR,HELIX,ERROR)
      ENDIF
      IF (ICODE.NE.0) RETURN
      IRET = UF2XYZ(X_ECAL,ERR_ECAL)

      DIST = SQRT((X_ECAL(1)-EXIT(1))**2 + (X_ECAL(2)-EXIT(2))**2
     &     + (X_ECAL(3)-EXIT(3))**2)
      IF (DIST.GT.2.)THEN
        CALL UCOPY(EXIT,X_ECAL,6)
        ERR_ECAL(1) = 0.
        ERR_ECAL(2) = 0.
      ENDIF
      X_ECAL(7) = PPINI
      X_ECAL(8) = Q

      UFECAL = 0

      RETURN
      END
#endif

