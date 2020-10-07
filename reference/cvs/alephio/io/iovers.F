      SUBROUTINE IOVERS (ALIBV,ALIOV)
C ------------------------------------------------------
CKEY IO VERSION
C - F.Ranjard - 940429
C! returns the IO version number
C
C - Output : - ALIBV / R = ALEPHLIB version #
C            - ALIOV / R = IO version #
C ------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "version.h"
C ------------------------------------------------------
      IW6 = IW(6)
      CALL ALVERS (ALIBV)
      IW(6) = IW6
      ALIOV = ALIOVER
      END
#endif
