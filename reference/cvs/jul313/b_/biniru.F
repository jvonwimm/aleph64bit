      SUBROUTINE BINIRU
C--------------------------------------------------
C!    Initialize RUN for BCAL
C
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
C
C?   No reconstruction for MC
C
      IF ( IW(NAMIND('AJOB')).NE.0 ) FDETRF(JULBC) = .FALSE.
  999 RETURN
      END
#endif
