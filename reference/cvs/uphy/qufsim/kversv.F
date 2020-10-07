        FUNCTION kversv(dummy)
C-----------------------------------------------------------------------
C! Create new vertex
C
C  Patrick Janot -- 23 Oct 1993
C-----------------------------------------------------------------------
#include "qcde.h"
C
C Enlarge ank QVRT
C
      IF ( klfrv .LE. klmcv )  CALL qsbank ('QVRT', klmcv + 50)
      knmcv  = knmcv + 1
      klmcv  = klmcv + 1
      kffrv  = klmcv+ 1
      kversv = klmcv
C
      RETURN
      END
