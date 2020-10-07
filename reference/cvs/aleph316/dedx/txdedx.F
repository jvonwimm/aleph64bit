      SUBROUTINE TXDEDX(BG,SBG,Q,RSIG,SMPL,DEDX,SDEDX,IER)
C----------------------------------------------------------------------
C! Routine for backward compatibility only. Call TXPDDX directly instead
C
C  M.Cattaneo 1999-07-29
C----------------------------------------------------------------------
#ifndef DOC
C
      LOGICAL FIRST /.TRUE./
#include "bcs.h"
      IF( FIRST ) THEN
        WRITE(IW(6),*)
     &   'Routine TXDEDX is obsolete. Please use TXPDDX instead'
        FIRST = .FALSE.
      ENDIF
C
      CALL TXPDDX('WIRE',BG,SBG,Q,RSIG,SMPL,DEDX,SDEDX,IER)
C
      RETURN
      END
#endif
