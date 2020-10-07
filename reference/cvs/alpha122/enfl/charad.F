      SUBROUTINE CHARAD(ICH)
C-------------------------------------------------------------------
C! Store charged particles in class KENFLW (-3)
C
C  Patrick Janot -- 19 May 1990
C-------------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "qcde.h"
#include "qhac.h"
#include "qmacro.h"
C-------------------------------------------------------------------
      gdmult = gdmult + 1.
      IF ( qch(ich) .GT. 0. ) chapo = chapo + 1
      IF ( qch(ich) .LT. 0. ) chane = chane + 1
      chatot = chatot + qch(ich)
      vec(1) = qx(ich)
      vec(2) = qy(ich)
      vec(3) = qz(ich)
      vec(4) = qe(ich)
      CALL addvec(vec,1,ich,0,1.)
      nch = NINT(gdmult)
      kan(nch) = ich
      RETURN
      END
#endif
