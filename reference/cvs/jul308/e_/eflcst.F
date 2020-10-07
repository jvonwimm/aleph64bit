      SUBROUTINE EFLCST
C******************************************************************
C!  GET E-Fow constants from d-base
C!  Read EFCA & EHDS banks
C!
C!  Author  :  M.N Minard  900110
C!
C!  BANKS:
C!    INPUT  : EFCA , EHDS
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
C-    Read EFCA
C
      LEFCA = ALGTDB(LRCONS,'EFCA',IRUNRC)
      IF ( LEFCA.EQ.0 ) THEN
         CALL RERROR( 'EFLCST',-1,' No E-Flow EFCA constants found')
      ENDIF
      LEHDS = ALGTDB(LRCONS,'EHDS',IRUNRC)
      IF ( LEHDS.EQ.0 ) THEN
         CALL RERROR( 'EFLCST',-2,' No E-Flow EHDS constants found')
      ENDIF
      RETURN
      END
#endif
