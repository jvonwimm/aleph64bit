      SUBROUTINE E4GTBR(A3,B3,C3,D3)
C----------------------------------------------------------------------
C!  - R2 ESTIMATOR : STOP EXTRAPOLATION AT BARREL CORNERS
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C?
C!======================================================================
#ifndef DOC
#include "e4com0.h"
#include "e4par1.h"
      D3=-ZLIMOD
      IF(ZI.LT.0)D3=-D3
      A3=0
      B3=0
      C3=1
      RETURN
      END
#endif
