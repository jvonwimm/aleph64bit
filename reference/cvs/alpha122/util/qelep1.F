      SUBROUTINE QELEP1(ELEP,IFLAG)
C----------------------------------------------------------------------
C! Get the run-averaged exact energy for 1993 -> 1995 LEP 1 runs
CKEY ENERGY/USER
C  Simple interface to ALEPHLIB routine GETLE1   J.Boucrot 26.08.97
C Output arguments :
C  ELEP  = LEP c.m.s. energy, in Gev
C  IFLAG = 2 if ELEP found from average run energy
C        = 3 in all other cases (then ELEP taken from bank LFIL)
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
C----------------------------------------------------------------------
      CALL GETLE1(ELEP,IFLAG)
      IF (IFLAG.EQ.3) ELEP=QELEP
      RETURN
      END
#endif
