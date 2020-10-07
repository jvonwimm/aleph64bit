      REAL FUNCTION KGDECL(P,AM,T0)
C-----------------------------------------------------------------------
C! Generates decay length in cm
CKEY KINE KINGAL DECAY /  INTERNAL
C      B.Bloch-Devaux        september 86
C
C - structure: REAL FUNCTION subprogram
C              User Entry Name: KBDECL
C              External References: RNDM(CERNLIB)
C              Comdeck referenced: ALCONS
C
C - Usage   : DECL = KGDECL (P,AM,T0)
C - Input   : P      = momentum of the particle
C             AM     = particle mass
C             T0     = particle life time
C - Output  : KGDECL = decay length in cm
#ifndef DOC
      SAVE
#include "alcons.h"
      PARAMETER (CLITS = CLGHT * 1.E+9)
      EXTERNAL RNDM
C ------------------------------------------------------------
      Z=RNDM(0)
      IF (Z.EQ.0.) Z=1.
      KGDECL=-P*CLITS*T0*ALOG(Z)/AM
      RETURN
      END
#endif
