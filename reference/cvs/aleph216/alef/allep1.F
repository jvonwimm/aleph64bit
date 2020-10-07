      LOGICAL FUNCTION ALLEP1 (IRUN)
C ----------------------------------------------------------------------
C!  Is this a LEP1 run?
CKEY ALEF LEP ENERGY
C - Steve Wasserbaech, 21 August 1995
C
C   This function tests the center-of-mass energy to determine
C   whether a run is a LEP1 run:
C     ALLEP1 = (sqrt(s) .LT. 100 GeV)
C
C   Calls: ALELEP
C
C - Input:
C   IRUN   / I  Run number
C
C - Output:
C   ALLEP1 / L  = .TRUE. if IRUN is a LEP1 run
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C
      INTEGER IRUN
      REAL ALELEP
C
C ----------------------------------------------------------------------
C
      ALLEP1 = (ALELEP(IRUN) .LT. 100.)
C
      RETURN
      END
#endif
