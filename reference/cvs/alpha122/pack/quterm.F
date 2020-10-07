      SUBROUTINE QUTERM
CKEY TERM /USER
C-----------------------------------------------------------------------
C! user termination
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
C
C       remove or modify the next statement :
      CALL QWMESS ('_QUTERM_ "Do nothing" version on ALPHA lib')
      END
#endif
