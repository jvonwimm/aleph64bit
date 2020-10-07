      SUBROUTINE QSUSTR (N)
CKEY USER TRACKS /USER
C----------------------------------------------------------------------
C! Reserve user space for tracks
C                                                   H.Albrecht 20.09.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C
      CALL QSBANK ('QVEC', N + KLFRT - KLUST)
      KLUST = N
C
      END
#endif
