      SUBROUTINE SIINIJO
C----------------------------------------------------------------------
C!  - SICAL Job Initialisation
C!
C!   Author   : B. Bloch-Devaux 911029
C!              Modified for Bunch train mode October 1994
C!
C!   Description :
C!   ===========
C?   Set name indices for Scal banks
C?   Init Lumi counters
C?   Reset reconstruction job statistics
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
      PARAMETER ( LSSTA = 9)
      COMMON/SILUMC/NSILEV(25,4)
C
C----------------------------------------------------------------------
C
C Name indices
      CALL SINAMX
C Lumi statistics
      CALL VZERO(NSILEV,100)
      RETURN
      END
#endif
