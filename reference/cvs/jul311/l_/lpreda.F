      SUBROUTINE LPREDA
C-----------------------------------------------------------------------
C!   Prepare Lcal data
C!
C!   Author   : P. H. Hansen  880801
C!
C!     Input   : 'LTDI' 'LWDI'        ( if Monte Carlo input)
C!     Input   : 'PLSD' 'PLPD' 'PLOB' ( if POT input)
C!     Input   : 'LAR2'               ( if Test Beam input)
C!     Output  : 'LSDA' 'LPDA' ('LOBJ')
C!     Created : 'LSDA' 'LPDA' ('LOBJ')
C!
C!     Description :
C!     ===========
C!       Transform raw data banks into
C!       prepared data banks.
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "lcnamc.h"
C ---------------------------------------------------------------------
      CALL BLIST(IW,'T+','LUPA')
C
C Monte Carlo input
      KLTDI = IW(NALTDI)
      IF(KLTDI.GT.0) THEN
         CALL LPREMC
         GOTO 999
      ENDIF
C
C POT input
      KPLSD = IW(NAPLSD)
      IF(KPLSD.GT.0) THEN
         CALL LPREPO
         GOTO 999
      ENDIF
C
C Test Beam input
      KLAR2 = NLINK('LAR2',0)
      IF(KLAR2.GT.0) THEN
         CALL LPRERW
      ENDIF
  999 CONTINUE
      END
#endif
