      SUBROUTINE TMDEDX(ITK,RI,RSIG,NS,TL,IER)
C----------------------------------------------------------------------
C! Routine for backward compatibility only. Call TMPDDX directly instead
C
C  M.Cattaneo 1999-07-29
C----------------------------------------------------------------------
#ifndef DOC
C
      LOGICAL FIRST /.TRUE./
#include "bcs.h"
      IF( FIRST ) THEN
        WRITE(IW(6),*)
     &   'Routine TMDEDX is obsolete. Please use TMPDDX instead'
        FIRST = .FALSE.
      ENDIF
C
      CALL TMPDDX('WIRE',ITK,RI,RSIG,XNS,TL,IER)
      NS = NINT(XNS)

      RETURN
      END
#endif
