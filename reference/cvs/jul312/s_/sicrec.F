      SUBROUTINE SICREC
C----------------------------------------------------------------------
C!  - SICAL Reconstruction
C!
C! Author   : B. Bloch-Devaux 911029 ,930215
C! Modified : M.Cattaneo      980226  Skip if setup code is zero (< 9209)
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
      SAVE ISETUP, IROLD
#include "rcurnt.h"
#include "rparac.h"
#include "rflags.h"
      INTEGER  GTSTUP
      EXTERNAL GTSTUP
      INTEGER ISETUP / 0 /, IROLD / 0 /
C---------------------------------------------------------------------
      IF( IROLD .NE. IRUNRC ) ISETUP = GTSTUP('SI',IRUNRC)
      IF( ISETUP .EQ. 0 ) GOTO 999

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
