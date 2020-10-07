      FUNCTION KFPADR (IPC)
CKEY PART /USER
C----------------------------------------------------------------------
C! Get row # in QFPA corresponding to particle IPC.
C!                                                  H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C
      KFPADR = IW(KOQPLI+IPC)
      IF (KFPADR .NE. 0)  GO TO 90
      KNQFPA = KNQFPA + 1
      IF (KNQFPA .GE. IW(KOQFPA+KCQFPA))
     &  CALL QSBANK ('QFPA', KNQFPA + 20)
      IW(KOQPLI+IPC) = KNQFPA
      KFPADR = KNQFPA
   90 END
#endif
