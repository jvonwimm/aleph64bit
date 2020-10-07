      SUBROUTINE ABRUEV (KRUN, KEVT)
C ------------------------------------------------------------------
C - F.Ranjard - 920226  from ABRREC/ENTRY ABRUEV
CKEY ALPHARD RUN EVENT / USER
C!  Returns current run and event number.
C   For event records : KRUN > 0 ; KEVT > 0.
C   For run records :   KRUN > 0 ; KEVT = 0.
C   For other records : KRUN = 0 ; KEVT = 0.
C --------------------------------------------------------------------
#ifndef DOC
#include "boscom.h"
C ---------------------------------------------------------------
      KRUN = NRUN
      KEVT = NEVT
      END
#endif
