      SUBROUTINE QUINIT
CKEY INIT /USER
C-----------------------------------------------------------------------
C! user initialisation
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
C-----------------------------------------------------------------------
C
C       remove or modify the next statement :
      CALL QWMESS ('_QUINIT_ "Do nothing" version on ALPHA lib')
      END
      SUBROUTINE QUEVNT (QT,KT,QV,KV)
CKEY EVENT /USER
C-----------------------------------------------------------------------
C! process one event
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      DIMENSION QT(KCQVEC,1), KT(KCQVEC,1), QV(KCQVRT,1), KV(KCQVEC,1)
#include "qmacro.h"
C-----------------------------------------------------------------------
C
C       remove or modify the next statements :
      IF (KNEVT .EQ. 1)  CALL QWMESS
     +   ('_QUEVNT_ Default version on ALPHA lib : print 3 events')
      CALL QWEVNT
      IF (KNEVT .EQ. 3)  CALL QMTERM ('0_QUEVNT_ Stop after 3 events')
      END
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
#endif
#endif
