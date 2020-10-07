      FUNCTION KPARTX (CINP,IFLG)
CKEY PART /INTERNAL
C----------------------------------------------------------------------
C!  Get particle code KPARTX for name CINP. IFLG = 0 if new particle.
C                                                   H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER * (*) CINP
C
      N = KNQPAR
      KPARTX = KPCOMP (CINP)
      IFLG = 0
      IF (N .EQ. KNQPAR)  IFLG = 1
C
      END
#endif
