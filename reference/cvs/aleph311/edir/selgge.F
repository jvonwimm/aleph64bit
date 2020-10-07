      SUBROUTINE SELGGE(TRKHOK)
C----------------------------------------------------------------------
CKEY EDIR GOOD GAMMA_GAMMA
C! Select good events according to gamma gamma group selection
C! Good event : number of good tracks  >= 3
C!                E(calorimeters)/ELEP <  0.5
C!                Charged Energy/ELEP  <  0.4
C-
C   Input  : none
C   Output : TRKHOK is true if good event for gamma gamma selection
C-
C   Called by   : SELEVT
C   Calls  : GGTRKH
C   Input banks : EVEH
C-
C                                      Author: A. Finch 23-May-1996
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "evehjj.h"
C --
      LOGICAL TRKHOK
      DATA LRUN/0/
C --
      TRKHOK = .FALSE.
C --
      KEVEH = IW(NAMIND('EVEH'))
      IF(KEVEH.LE.0) GOTO 999
C --
C   Get LEP energy for run number NRUN
C --
      NRUN = IW(KEVEH+JEVERN)
      NEVT = IW(KEVEH+JEVEEV)
      IF(NRUN.NE.LRUN) THEN
        LRUN = NRUN
        ENLEP = ALELEP(NRUN)
        IF(ENLEP.LT.80. .OR. ENLEP.GT.300.) ENLEP = 200.0
      ENDIF
C --
C   Get the number of good tracks NGDTR and the sum of their energy
C   ECHRG
C --
      CALL GGTRKH(NGDTR,ECHRG)
      FECHRG = ECHRG/ENLEP
      IF(FECHRG.GT.2.0)FECHRG=0.1
C --
C   Store wire energy
C --
      CALL GGESUM
C --
C  Get the total calorimetric energy
C --
      CALL GGSEH (ETOT)
      FECAL = ETOT /ENLEP
      IF(FECAL.GT.2.0)FECAL = 0.1
C --
C  Apply the selection cuts
C --
      IF(NGDTR.GE.3 .AND. FECAL.LT.0.5
     1 .AND. FECHRG .LT.0.4) TRKHOK = .TRUE.
 
C --
999   RETURN
      END
#endif
