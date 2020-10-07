      SUBROUTINE QJFOXW (FOX,ICLASS)
CKEY SHAPE /USER
C----------------------------------------------------------------------
C   Author   : E. Blucher     2-APR-1989
C
C   Description
C   ===========
C!  Set up and call Fox-Wolfram routine.
C======================================================================
#ifndef DOC
      DIMENSION FOX(5)
#include "qcde.h"
#include "qctbuf.h"
C-----------------------------------------------------------------------
      CALL QJSETU(ICLASS)
C
      CALL QGFOXW
      FOX(1)=QTBOR(1)
      FOX(2)=QTBOR(2)
      FOX(3)=QTBOR(3)
      FOX(4)=QTBOR(4)
      FOX(5)=QTBOR(5)
      END
#endif
