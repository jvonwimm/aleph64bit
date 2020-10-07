      SUBROUTINE QJSPHE (SPHER,CNAM,ICLASS)
CKEY SHAPE /USER
C----------------------------------------------------------------------
C   Author   : E. Blucher     27-FEB-1989
C
C   Description
C   ===========
C!  Set up and call sphericity routine.
C======================================================================
#ifndef DOC
      CHARACTER *(*) CNAM
#include "qcde.h"
#include "qctbuf.h"
C-----------------------------------------------------------------------
      CALL QJSETU(ICLASS)
C
      CALL QGSPHE
      SPHER = 1.5 * (1. - QTBOR(1))
      IF (CNAM.NE.' ')  CALL QJSAVE (CNAM,1,ICLASS,0,0,0)
      END
#endif
