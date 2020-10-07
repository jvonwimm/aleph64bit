      SUBROUTINE MQUNEWR
CKEY RUN /INTERNAL
C ----------------------------------------------------------------------
C MINI-DST official production : new run      
C Put in ALPHA 8-Jan-1999
C Called from QMINIT
C ----------------------------------------------------------------------
#ifndef DOC
#include "minprd.h"
#include "qcde.h"
      DIMENSION SUMRY(7)
C ----------------------------------------------------------------------
C Do-nothing if not official MINI production :
      IF (IMIPRD.NE.1) GO TO 999
C Print summary for last run, and reinitialise.
C (KRUN = 0 at end of processing.)
C
      IF (KRUN.EQ.0) GO TO 999
      IF (KRUN.EQ.LRUN) GO TO 999
      IF (LRUN.GT.0) THEN
         CALL MINMON(+1,0, SUMRY)
         CALL MINSUM(+1,LRUN,SUMRY)
      ENDIF
      CALL PRRHAH
      LRUN = KRUN
 999  RETURN
      END
#endif
