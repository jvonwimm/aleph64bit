      SUBROUTINE RZERUN
C--------------------------------------------------------------------
C! Zero statistics arrays which count inside each run
C?
C!======================================================================
#ifndef DOC
#include "rstate.h"
#include "ravtim.h"
C--------------------------------------------------------------------
      CALL VZERO(NEVTRS,MESTA)
      CALL VZERO(NEVTRR,MEERR)
      STIMRA = 0.0
C--------------------------------------------------------------------
      END
#endif
