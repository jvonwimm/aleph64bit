      SUBROUTINE TWIRFX
C
C----------------------------------------------------------------------
C! Standardize TPC wire raw data banks for Monte Carlo
C
C  R. Johnson    17-01-91
C
C  The versions of TPCSIM earlier than 208 produce banks TWIR, TWDI
C  which are reduced.  The reduced banks now are supposed to be named
C  TRIR and TRDI.  This routine is called by TPREDA to rename them.
C
C?
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "tsimjj.h"
C
      DATA NTSIM/0/
C
#include "bmacro.h"
C
C++   Rename the banks to the original names for old versions
C
      IF (NTSIM.EQ.0) NTSIM=NAMIND('TSIM')
      KTSIM=IW(NTSIM)
      IF (KTSIM.EQ.0) RETURN
C
      IVER=ITABL(KTSIM,1,JTSITV)
      IF (IVER.LE.208) THEN
        CALL BSWAP(IW,'TWIR','TRIR')
        CALL BSWAP(IW,'TWDI','TRDI')
      ENDIF
C
      RETURN
      END
#endif
