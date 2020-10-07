       SUBROUTINE MQUINIT
CKEY INIT/INTERNAL
C ----------------------------------------------------------------------
C MINI-DST official production initialisation
C Put in ALPHA 8-Jan-1999
C Called from QMINIT
C ----------------------------------------------------------------------
#ifndef DOC                                                        
#include "minprd.h"
      DIMENSION SUMRY(7)
#include "qcde.h"
C ----------------------------------------------------------------------
C Do-nothing if not official MINI production :
      IF (IMIPRD.NE.1) GO TO 999
C
C Initialise Mini monitoring.
C
      LRUN = -999
      CALL MINMON(-1,0, SUMRY)
      CALL MINSUM(-1,LRUN,SUMRY)
C
C Check if we want printout.
C
      IPRNT = 1
      IF (NLINK('MPRI',0).GT.0) IPRNT = IW(NLINK('MPRI',0)+1)
      CALL HTITLE(' ')
C
      ICOUNT = 0
 999  RETURN
      END
#endif
