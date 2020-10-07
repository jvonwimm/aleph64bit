      SUBROUTINE QWCLAS(IC)
CKEY WRITE EDIR /USER
C----------------------------------------------------------------------
C! Set bits for event directory classification word.
C
C                                          Authors: E. Blucher 16.09.89
C                                                   M. Talby
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CALL ABCLAS (IC)
      IF (.NOT. XWREVT)  CALL QWRITE
      END
#endif
