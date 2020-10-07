      SUBROUTINE EGFCST
C******************************************************************
C!  Get Correction constant for gas flow, Read ECGF bank
C!
C!  Author  :  M.N Minard  910915
C!
C!  BANKS:
C!    INPUT  : ECGF
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "rparac.h"
#include "rflags.h"
      INTEGER ALGTDB
C
C-    Read ECGF
C
      LECGF = ALGTDB(LRCONS,'ECGF',IRUNRC)
      IF ( LECGF.EQ.0 ) THEN
         CALL RERROR ( 'EGFCST', 1, ' No  ECGF constants found')
      ENDIF
      RETURN
      END
#endif
