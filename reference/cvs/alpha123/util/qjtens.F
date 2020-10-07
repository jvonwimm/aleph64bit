      SUBROUTINE QJTENS(EVAL,CNAM,ICLASS)
CKEY SHAPE /USER
C----------------------------------------------------------------------
C   Author   : E. Blucher     30-JUN-1989
C
C   Description
C   ===========
C!  Set up and call routine to calculate normalized momentum tensor.
C
C   KPDIR(CNAM,ICLASS) = major axis (sphericity axis)
C   KFOLLO (...) = semi-major axis
C   KFOLLO (...) = minor axis
C   EVAL(1,2,3) = eigenvalues in descending order
C         sphericity = 1.5*(1-EVAL(1))
C         aplanarity = 1.5*EVAL(3)
C         planarity = EVAL(3)/EVAL(2)
C======================================================================
#ifndef DOC
      CHARACTER *(*) CNAM
      DIMENSION EVAL(3)
#include "qcde.h"
#include "qctbuf.h"
C-----------------------------------------------------------------------
      CALL QJSETU(ICLASS)
C
      CALL QGTENS
      EVAL(1) = QTBOR(1)
      EVAL(2) = QTBOR(2)
      EVAL(3) = QTBOR(3)
   90 IF (CNAM .NE. ' ')  THEN
        ICL = KSCOLD (ICLASS)
        CALL QJSAVE (CNAM,3,ICLASS,0,0,0)
        CALL QJSAVE (CNAM,2,ICLASS,1,0,0)
        CALL QJSAVE (CNAM,1,ICLASS,1,0,0)
      ENDIF
  999 RETURN
      END
#endif
