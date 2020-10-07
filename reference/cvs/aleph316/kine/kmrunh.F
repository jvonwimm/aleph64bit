      INTEGER FUNCTION KMRUNH (NRUN,NEXPE,IRTYP)
C ---------------------------------------------------------
C - F.Ranjard - 870604
C! Modify RUNH parameters
CKEY KINE KINGAL FILL BANK   /  USER  INTERNAL
C  RUNH bank has been filled in ALRUNH with default parameters
C  which can be overwritten calling this subroutine
C  first Drop RUNR and RUNH banks, then recreate them
C
C - structure : INTEGER FUNCTION subprogram
C               User Entry Name: KMRUNH
C               External References: BDROP(BOS77)
C               Comdecks referenced: BCS
C
C - usage    : JRUNH = KMRUNH (NRUN,NEXPE,IRTYP)
C - input    : NRUN  = run #
C              NEXPE = experiment #
C              IRTYP = run type
C - output   : KMRUNH= RUNH bank index
C                      0 means RUNH or RUNR does not exist
#ifndef DOC
      SAVE
#include "bcs.h"
      INTEGER ALRUNH, ALRUNR
C ----------------------------------------------------------
C - drop RUNH bank
      CALL BDROP (IW,'RUNRRUNH')
C
C - create RUNH bank
      JRUNH = ALRUNH (NRUN,NEXPE,IRTYP)
      JRUNR = ALRUNR (NEXPE,NRUN)
C
      KMRUNH = JRUNH
      IF (JRUNR .EQ. 0) KMRUNH = 0
C
      END
#endif
