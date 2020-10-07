      SUBROUTINE SICREC
C----------------------------------------------------------------------
C!  - SICAL Reconstruction
C!
C! Author   : B. Bloch-Devaux 911029 ,930215
C!
C!   Description:
C!   ============
C?   Call prepare luminosity routine  SILHCR
C?   Call clustering routine          SICLUS
C?   Call debug routine on request    SIBKPR
C?   Call Luminosity routine          SILUMI
C?   Call statistics routine          SISTAT
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
C---------------------------------------------------------------------
C - Prepare for Luminosity   banks
      CALL SILHCR(IER)
C
C - Do clustering
      CALL SICLUS
C
C - Do Sical Luminosity
      CALL SILUMI
C - Dump event on request
      IF(FDEBRF.AND.JDBDRF(JULSI).GE.1) CALL SIBKPR
C
C - Fill statistics bank
      CALL SISTAT
C
  999 CONTINUE
      END
#endif
