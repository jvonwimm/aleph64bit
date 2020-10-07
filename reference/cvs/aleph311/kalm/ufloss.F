      SUBROUTINE UFLOSS(STATE, JACO, MASS, DE)
C! Integrate continuous energy loss in a thick layer and apply to state
C
C   D. Casper
C               Modified May 2, 1997 to add sanity check on energy loss
C

#ifndef DOC
      DOUBLE PRECISION STATE(5), JACO(5,5), MASS, DE

      DOUBLE PRECISION P_OLD, E_OLD, E_NEW, P_NEW, OMEGA_NEW
      DOUBLE PRECISION D_OMEGA
#include "uftcom.h"

      IF (DE .EQ. 0.D0) RETURN

C  Calculate change in curvature resulting from this change in energy.

      P_OLD = DABS(1.D0/(DBLE(ROVERP) * STATE(5) * DCOS(STATE(4))))
      E_OLD = DSQRT(P_OLD**2 + MASS**2)
      IF (ABS(DE) .GT. 0.05 * E_OLD) DE = 0.D0
      E_NEW = E_OLD + DE
      P_NEW = DSQRT(E_NEW**2 - MASS**2)
      OMEGA_NEW = 1.D0/(DCOS(STATE(4))*DBLE(ROVERP)*P_NEW)
C       D_OMEGA = OMEGA_NEW - DABS(STATE(5))
      D_OMEGA = DSIGN(OMEGA_NEW - DABS(STATE(5)),STATE(5))
      STATE(5) = DSIGN(OMEGA_NEW,STATE(5))

C  Now calculate the change in each of the other components of the state vector

      STATE(1) = STATE(1) + 0.5D0 * JACO(1,5) * D_OMEGA
      STATE(2) = STATE(2) + 0.5D0 * JACO(2,5) * D_OMEGA
      STATE(3) = STATE(3) + 0.5D0 * JACO(3,5) * D_OMEGA

      RETURN
      END
#endif
