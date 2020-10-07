      SUBROUTINE QSTFLR(ITK,IFLAG,VALUE)
CKEY FLAG /USER
C----------------------------------------------------------------------
C   Author   : E. Blucher      7-APR-1989
C   Description
C   ===========
C!  Set flag IFLAG in user space to VALUE (real) for ITK.
C!======================================================================
#ifndef DOC
#include "qcdesh.h"
      JQVEC(I,N) = KOQVEC + I*KCQVEC + N
C----------------------------------------------------------------------
C
      IF(IFLAG.LT.1..OR.IFLAG.GT.KCQVEC-JQVEUS+1)THEN
      CALL QWMESE('_QSTFLR_ IFLAG out of range -- flag not set.')
      GOTO 999
      ENDIF
      RW(JQVEC(ITK,JQVEUS + IFLAG - 1)) = VALUE
  999 CONTINUE
      END
#endif
