      SUBROUTINE MQUTERM
CKEY TERM/INTERNAL
C ----------------------------------------------------------------------
C MINI-DST official production : Termination of job
C Put in ALPHA 8-Jan-1999
C Called from QMTERM
C ----------------------------------------------------------------------
#ifndef DOC
#include "minprd.h"
#include "qcde.h"
      DIMENSION SUMRY(7)
C ----------------------------------------------------------------------
C Do-nothing if not official MINI production:
      IF (IMIPRD.NE.1) GO TO 999
C Termination, including printing summary for last run.
C
      CALL ABUNIT(LDST,LSEL,LSEL2,LMINI,LEDIR)
      CALL BWRITE(IW,LMINI,'0')
      CALL MINMON(+1,0, SUMRY)
      CALL MINSUM(+2,LRUN,SUMRY)
C Print histogram and close files :
      CALL HPRINT(0) 
      CALL ACLOSE(0,IER)
 999  RETURN
      END
#endif
