*DK agbeam
      SUBROUTINE AGBEAM
C---------------------------------------------
C!   Build beam pipe geometry up to compensator parts
C                              B. BLOCH-DEVAUX 19 MARCH 85
C     modified 15 april 86  for revised implementation
C     modified sept 87 to implement Data Base  Banks
C     modified Novem 90 to implement New Beam pipe from 91 onwards
C     modified January 92 for 92 beam pipe and complete rewriting...
CKEY BPIPE GEOM
C - called by AGEOME
C - calls     AGGEAN                            from this Lib
C - calls     ALTELL                            from ALEPHLib
C-------------------------------------------------------------------
#ifndef DOC
#include "jobcom.h"
C - get setup code and build BPIP from data base banks
C
      CALL AGGEAN ('BP',IBPST)
C
      IF (IBPST.LT.1)
     & CALL ALTELL('Beam Pipe Data still missing...STOP',0,'STOP')
      RETURN
      END
#endif
