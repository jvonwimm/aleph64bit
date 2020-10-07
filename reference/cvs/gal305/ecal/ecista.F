*DK ecista
      SUBROUTINE ECISTA
C.----------------------------------------------------------------
C  M.Rumpf jan 86
C! Initialise ECAL Statistics
C   -Called by ECIRUN
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ecstat.h"
C
      DO 1 I=1,NECST
        NECONT(I) = 0.
        ECCONT(I) = 0.
    1 CONTINUE
C
      END
#endif
