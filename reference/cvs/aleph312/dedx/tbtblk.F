      FUNCTION TBTBLK(BGL,Q,SMPL,IER)
C----------------------------------------------------------------------
C! Routine for backward compatibility only. Call TBTBLP directly instead
C
C  M.Cattaneo 1999-07-29
C----------------------------------------------------------------------
#ifndef DOC
C
      LOGICAL FIRST /.TRUE./
#include "bcs.h"
      IF( FIRST ) THEN
        WRITE(IW(6),*)
     &   'Function TBTBLK is obsolete. Please use TBTBLP instead'
        FIRST = .FALSE.
      ENDIF
C
      TBTBLK = TBTBLP('WIRE',BGL,Q,SMPL,IER)
C
  999 RETURN
      END
#endif
