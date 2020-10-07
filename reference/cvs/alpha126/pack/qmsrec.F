      SUBROUTINE QMSREC
CKEY RUN /INTERNAL
C----------------------------------------------------------------------
C! called for "unknown" record types (slow control, end-of-run)
C called from QMREAD
C                                         Author: E.Blucher  10.09.89
C  Modified for MINI end-of-run records           J.Boucrot  10.01.99
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
#include "minprd.h"
C----------------------------------------------------------------------
C Write end-of-run record for MINI MCarlo datasets:
      IF (XWMINI.OR.(IMIPRD.EQ.1)) CALL MQUSREC
C User's processing :
      CALL QUSREC
   90 END
#endif
