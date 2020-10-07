      SUBROUTINE QELEP2(ELEP,IFLAG)
C----------------------------------------------------------------------
C! Get the "instantaneous" exact LEP energy for LEP 2 runs
CKEY ENERGY/USER
C  Simple interface to ALEPHLIB routine GETLE2   J.Boucrot  26.3.97
C Output arguments :
C  ELEP  = LEP c.m.s. energy, in Gev
C  IFLAG = 1 if ELEP found from official time-slice LEP information
C        = 2 if ELEP found from average run energy
C        = 3 in all other cases (then ELEP taken from bank LFIL)
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
C----------------------------------------------------------------------
      CALL GETLE2(ELEP,IFLAG)
      IF (IFLAG.EQ.3) ELEP=QELEP
      RETURN
      END
#endif
