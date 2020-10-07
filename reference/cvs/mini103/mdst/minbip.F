      SUBROUTINE MINBIP
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Swap bank DBTG and NBIP.
C
C     Author: Agnieszka Jacholkowska 1-Oct-94
C
C     Input  : DBTG bank
C     Output : NBIP bank
C
C     Called by ALPHA
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      LOGICAL GOTIT
C
C!    set of intrinsic functions to handle BOS banks
#include "bmacro.h"
C
C
C++   Swap DBTG bank into NBIP bank.
C
      KNBIP =  NSWAP('DBTG', 1,'NBIP',1)
      CALL BLIST(IW,'E+','NBIP')
      IF(KNBIP.LE.0) GOTO 1000
C
C
 1000 CONTINUE
C
      RETURN
      END
#endif
