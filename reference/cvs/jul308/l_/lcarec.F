      SUBROUTINE LCAREC
C----------------------------------------------------------------
C! Controls Lcal pattern recognition
C! Author   : P. H. Hansen   880315
C!
C!   Description:
C!   ============
C?   Call clustering routine
C?   Call electron identification routine
C?   Call debug routine on request
C----------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
C---------------------------------------------------------------------
C
C Do clustering
      CALL LCLUST
C
C - Dump event on request
      IF(FDEBRF.AND.JDBDRF(JULLC).GE.1) CALL LBNKPR
C
C Find electron candidates
      CALL LELECT
C
  999 CONTINUE
      END
#endif
