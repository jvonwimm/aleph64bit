      SUBROUTINE UFDISC(STATE, MASS, DE)
C!  Apply discrete energy loss in thin layer of material to state vector
C
C
C       D, Casper
C                   Modified May 2, 1997 to add sanity check on energy loss
C
#ifndef DOC
      DOUBLE PRECISION STATE(5), MASS, DE

      DOUBLE PRECISION P_OLD, E_OLD, E_NEW, P_NEW, OMEGA_NEW
#include "uftcom.h"

      IF (DE .EQ. 0.D0) RETURN

C  Calculate change in curvature resulting from this change in energy.

      P_OLD = DABS(1.D0/(DBLE(ROVERP) * STATE(5) * DCOS(STATE(4))))
      E_OLD = DSQRT(P_OLD**2 + MASS**2)
      IF (ABS(DE) .GT. 0.05*E_OLD) DE = 0.
      E_NEW = E_OLD + DE
      P_NEW = DSQRT(E_NEW**2 - MASS**2)
      OMEGA_NEW = 1.D0/(DCOS(STATE(4))*DBLE(ROVERP)*P_NEW)
      STATE(5) = DSIGN(OMEGA_NEW,STATE(5))

      RETURN
      END
#endif
