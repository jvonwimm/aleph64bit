      SUBROUTINE MQUEVNT
CKEY EVENT/INTERNAL
C ----------------------------------------------------------------------
C MINI-DST official production Event Processing
C Put in ALPHA 8-Jan-1999
C Called from QMEVNT
C ----------------------------------------------------------------------
#ifndef DOC
#include "minprd.h"
#include "qcde.h"
      DIMENSION SUMRY(7)
      CHARACTER*800 ELIST,RLIST
#include "qmacro.h"
C ----------------------------------------------------------------------
C Do-nothing if not official MINI production:
      IF (IMIPRD.NE.1) GO TO 999
C Create Mini-DST for event records.
      CALL MINDST(ELIST,RLIST)
C
      ICOUNT = ICOUNT + 1
C Call special algorithms for the MINI :
      CALL MINALGO
C
      CALL MINHIS(IMAQQ)
      CALL MINOUT
      CALL MINMON(0,IMAQQ, SUMRY)
      IF (IPRNT.GE.1) CALL AUBLIS('T')
C
 999  RETURN
      END
#endif
