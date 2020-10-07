      SUBROUTINE HCCYL(POINT,RL,THETAL,PHIL)
C------------------------------------------------------------
C
CKEY HCALDES HCAL TRANSFORM COORDINATES / USER
C!  Trasforms the coordinates from 'ALEPH'  to 'CYLINDRICAL' Ref.Sys
C!
C!          Author    :G.Catanesi  85/12/11
C!
C!          input:
C!               - POINT/R   : point in the 'ALEPH' frame
C!          output:
C!               - RL,THETAL,PHIL : point in the 'CYLINDRICAL' frame
C!
C!          Called by: HCCRTO
C!          Calls    : none
C!-----------------------------------------------------------------
C.
C.
C.    ------------------------------------------------------------------
#ifndef DOC
C.
#include "alcons.h"
#include "hcgeom.h"
C
      DIMENSION POINT(*)
C
C. Compute Phi
C.
      PHIL=ATAN2(POINT(2),POINT(1)) - HCPHOF
      IF(PHIL.LT.0)PHIL = PHIL + TWOPI
      IF(PHIL.GT.TWOPI) PHIL=PHIL-TWOPI
C. Compute Theta
C.
      RL    = SQRT(POINT(1)**2+POINT(2)**2)
      THETAL  = ATAN2(RL,POINT(3))
C
      RETURN
      END
#endif
