      SUBROUTINE TIDHYP(ITK,FIELD,N,RMASS,Q,RI,NS,TL,RIEXP,SIGMA,IER)
C----------------------------------------------------------------------
C! Routine for backward compatibility only. Call TPDHYP directly instead
C
C  M.Cattaneo 1999-07-29
C----------------------------------------------------------------------
#ifndef DOC
C
      LOGICAL FIRST /.TRUE./
#include "bcs.h"
      IF( FIRST ) THEN
        WRITE(IW(6),*)
     &   'Routine TIDHYP is obsolete. Please use TPDHYP instead'
        FIRST = .FALSE.
      ENDIF
C
      CALL TPDHYP('WIRE',ITK,FIELD,N,RMASS,Q,RI,XNS,TL,RIEXP,SIGMA,IER)
      NS = NINT(XNS)
C
      RETURN
      END
#endif
