      SUBROUTINE  VDGEPH(R,TL,P0,D0,Z0,P,X)
C-----------------------------------------------------
C! physics parameter from helix parameters
C!
C!    SUBROUTINE  VDGEPH
C!
C!    by Hans-Guenther Moser
C!
C!    created:           18/03/88
C!
C!    last modification: 18/03/88
C!
C!    -----------------------------------------
C!
C!     input:  R      radius
C!             TL     dip angle
C!             P0     Phi at closest point to origin
C!             D0     distance to x=y=0 in x-y plane
C!             Z0     z coordinate at D0
C!
C!     output  P(1-3) Momentum vector
C!             X(1-3) coordinates of D0i
C!
C     =========================================
#ifndef DOC
C
#include "rconds.h"
      REAL P(*),X(*)
C
C
C     conversion radius of track <=> momentum
C     radius in meter , B in Tesla, p in GeV/c  q in units of e
C
C      p = 0.29979 * q * B * r
C
C     R[cm] = ROFP * P[Gev/c]:
C
      ROFP = 1./ (0.29979 * FIELRC / 10.) * 100.
      POFR = 1./ ROFP
C
      PT = POFR * ABS(R)
C
      P(1) = PT * COS(P0)
      P(2) = PT * SIN(P0)
      P(3) = PT * TL
C
      X(1) = D0 * SIN(P0)
      X(2) = - D0 * COS(P0)
      X(3) = Z0
C
      RETURN
      END
#endif
