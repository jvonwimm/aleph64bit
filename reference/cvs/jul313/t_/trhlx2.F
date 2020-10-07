      SUBROUTINE TRHLX2(RAD,PAR,V,IERR)
C
C----------------------------------------------------------------------
C! Intersection of helix with cylinder
C!
C!   Author:   M Mermikides  6-10-86
C!   Modified: D.Smith      99/10/27 Double precision fixes for Linux
C!
C!   Input:
C!      PAR:    /R      Helix parameters Q*Rho,tanl,phi0,d0,z0
C!      RAD:    /R      Radius of cylinder centered at zero
C!   Output:
C!      V:      /R      1 intersection points, x,y,z,phi
C!      IERR:   /I      =1 if intersection is not physical
C!
C!   Description:
C!   -----------
C!   Computes intersections of first full turn of helix described by
C!   parameters PAR (Q*Rho, tanl, phi0, d0, z0) with cylinder of radius
C!   RAD centred at zero.   Intersection points returned in
C!   V(x,y,z,phi, 2)
C!
C!   Called by TRPROJ
C!
C----------------------------------------------------------------------
#ifndef DOC
C
#include "alcons.h"
      DOUBLE PRECISION RHO,PHI0,D0,RADSQ,ARG,ALPHA,APSI
C
      DIMENSION PAR(*)
      DIMENSION V(4,*)
C
      IERR = 0
      RHO = ABS(PAR(1))
      Q = SIGN (1.0,PAR(1))
      PHI0 = PAR(3)
      D0 = PAR(4)
      Z0 = PAR(5)
      TANL = PAR(2)
      RADSQ = RAD*RAD
      ARG = (D0*(2D0*RHO-D0) - RADSQ)/(2.*RAD*(RHO-D0))
      IF (ABS(ARG).GT.1D0) THEN
         IERR = 1
      ELSE
         ALPHA = ASIN(ARG)
         PHI = PHI0 + Q*ALPHA
         V(1,1) = RAD*COS(PHI)
         V(2,1) = RAD*SIN(PHI)
         ARG = (RADSQ-D0*D0)/(4D0*RHO*(RHO-D0))
         IF (ARG.LT.0D0) THEN
           APSI = 0D0
         ELSE
           APSI = SQRT(ARG)
         ENDIF
         IF (APSI.GT.1D0) APSI = 1D0
         SS = 2D0*RHO*ASIN(APSI)
         V(3,1) = PAR(5) + SS*TANL
         IF (PHI.GT.TWOPI)   PHI=PHI-TWOPI
         IF (PHI.LT.0.)      PHI=PHI+TWOPI
         V(4,1) = PHI
C
         PHI = PHI0 + DBLE(PI) - Q*ALPHA
         V(1,2) = RAD*COS(PHI)
         V(2,2) = RAD*SIN(PHI)
         V(3,2) = PAR(5) + (DBLE(TWOPI)*RHO -SS)*TANL
         IF (PHI.GT.TWOPI)   PHI=PHI-TWOPI
         IF (PHI.LT.0.)      PHI=PHI+TWOPI
         V(4,2) = PHI
      ENDIF
      RETURN
      END
#endif
