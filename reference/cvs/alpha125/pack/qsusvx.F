      SUBROUTINE QSUSVX (N)
CKEY VERTEX USER /USER
C----------------------------------------------------------------------
C! Reserve user space for vertices
C                                                   H.Albrecht 20.09.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C
      CALL QSBANK ('QVRT', N + KLFRV - KLUSV)
      KLUSV = N
C
      END
#endif
