      SUBROUTINE GASTEER
C----------------------------------------------------------------------
C!  - Steering for photon
C!
C!   Author   :- M.N. Minard            7-APR-1993
C!               Mod 10.10.94
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
C...  Build PECO #1 (event vertex corrected bank)
      CALL GAPECO(IER)
C...  Build PGID bank
      CALL GAPGID
C...  Build PGPC bank (Bulos+Gampex)
      CALL GAPGAC
      RETURN
      END
#endif
