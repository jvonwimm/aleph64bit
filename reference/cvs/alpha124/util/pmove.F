      SUBROUTINE PMOVE(POINT,TPAR)
CKEY MATCHING / INTERNAL
C-------------------------------------------------------------------------------
C! Re-define the track parameters relative to a new point in space.
C  Auxiliary to JULMATCH
C  Author                                        Dave Brown 4-9-93
C  Inputs arguments :
C
C     POINT(3) = point in space
C     TPAR(5)  = track parameters
C
C  Outputs arguments :
C      TPAR  updated
C-------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "alcons.h"
C
C  Inputs; point in space, track parameters
C
      REAL POINT(3),TPAR(5)
C
C  Outputs;
C      TPAR  updated
C
C  Local variables
C
      INTEGER ISOL
      INTEGER NPI
      REAL DELTA,RADIUS
      REAL OMEGA,TANL,PHI0,D0,Z0,COSP0,SINP0,DPHI,PHI,SDIST
      REAL NEWD0,MIND0
      REAL OFFSET(2) /0.0 , 3.1415926 /
C-------------------------------------------------------------------------------
C  Put track parameters into local variables
C
      OMEGA = TPAR(1)
      TANL= TPAR(2)
      PHI0 = TPAR(3)
      D0  = TPAR(4)
      Z0  = TPAR(5)
      RADIUS = 1.0/OMEGA
      DELTA  = RADIUS - D0
      COSP0 = COS(PHI0)
      SINP0 = SIN(PHI0)
      DPHI = ATAN2(DELTA*SINP0+POINT(1),DELTA*COSP0-POINT(2))
      IF(DPHI-PHI0.GT.PI)  DPHI = DPHI-TWOPI
      IF(DPHI-PHI0.LT.-PI) DPHI = DPHI+TWOPI
C
C  Try both tangent branches explicitly
C
      MIND0 = 1000000000.0
      DO ISOL=1,2
        PHI = DPHI + OFFSET(ISOL)
        IF(ABS(SIN(PHI)).GT.0.5)THEN
          NEWD0 = RADIUS -(DELTA*SINP0 + POINT(1))/SIN(PHI)
        ELSE
          NEWD0 = RADIUS -(DELTA*COSP0 - POINT(2))/COS(PHI)
        END IF
        IF (ABS(NEWD0).LT.MIND0) THEN
          MIND0 = ABS(NEWD0)
C
C  Choose the best 2pi wrap for the Z distance
C
          NPI = NINT( (OMEGA*(POINT(3)-Z0)/TANL - (PHI-PHI0))/TWOPI )
          TPAR(3) = PHI + NPI*TWOPI
          TPAR(4) = NEWD0
          SDIST = (TPAR(3)-PHI0)/OMEGA
          TPAR(5) = Z0 + SDIST*TANL - POINT(3)
        END IF
      END DO
C
C  Done
C
      RETURN
      END
#endif
