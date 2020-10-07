      SUBROUTINE RZERGL
C--------------------------------------------------------------------
C! Block Data and zero of arrays
C!
C!   Author       :- J. Bunn
C!
C!   Modified     :- S. Orteu 87/1/13
C!
C!
C?
C!======================================================================
#ifndef DOC
#include "ravtim.h"
#include "revent.h"
#include "rlunit.h"
#include "rstatr.h"
C--------------------------------------------------------------------
C Some job parameters
C
      TIMJRA = 0.
      TKBYRE = 0.
C
      CALL VZERO(NRUNRS,MRSTA)
      CALL VZERO(NRUNRR,MRERR)
C
      END
#endif
