      FUNCTION TDXERR(NS,TL,IER)
C----------------------------------------------------------------------
C! Routine for backward compatibility only. Call TPXERR directly instead
C
C  M.Cattaneo 1999-07-29
C----------------------------------------------------------------------
#ifndef DOC
C
      LOGICAL FIRST /.TRUE./
#include "bcs.h"
      IF( FIRST ) THEN
        WRITE(IW(6),*)
     &   'Function TDXERR is obsolete. Please use TPXERR instead'
        FIRST = .FALSE.
      ENDIF
C
      XNS = REAL(NS)
      TDXERR = TPXERR('WIRE',XNS,TL,IER)
C
      RETURN
      END
#endif
