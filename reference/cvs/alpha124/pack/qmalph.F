      SUBROUTINE QMALPH
CKEY INIT /INTERNAL
C----------------------------------------------------------------------
C! initialise ALPHA
C! called from QMINIT
C!                                                  H.Albrecht 20.09.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      INTEGER BKINCA
C
C       initial number of tracks / vertices in QVEC / QVRT
      PARAMETER (LQVEC = 300, LQVRT = 100)
C
C       initial length of QLIN, QDET
      PARAMETER (LQLIN = 1000, LQDET = 150)
C
C       create QVEC bank
C
      CALL QSBANK ('QVEC', LQVEC)
C
C       create QVRT bank
C
      CALL QSBANK ('QVRT', LQVRT)
C
C      dummy detector bank : content = 0.
C
      KQZER = NBANK ('QZER', 0, 99)
C
C       create QDET bank.  1st row is 0 or points to a zero bank
C
      CALL QSBANK ('QDET', LQDET)
C
      CALL QSBANK ('QTRA', 0)
      CALL QSBANK ('QMTX', 1)
      CALL QSBANK ('QFPA', 0)
      CALL QSBANK ('QLIN', LQLIN)
      KNQLIN = 0
      KQWRK = NBANK ('QWRK', 0, 300)
C
C Put the ALPHA version number in a bank :
      CALL AUBOS('ALPV',0,1,JALPV,IGARB)
      IF (JALPV.GT.0) IW(JALPV+1)=BKINCA(CQVERS(1:3))
      END
#endif
