        SUBROUTINE SELTRK(TRKHOK)
C----------------------------------------------------------------------
CKEY EDIR GOOD TRACKS
C! Select good events according to QQbar group selection.
C! Good event = number of good tracks >=5 + E(track)/ELEP > 10.
C-
C   Input  : none
C   Output : TRKHOK is true if good event for QQbar selection
C-
C   Called by   : SELEVT
C   Calls  : TRKHAD
C   Input banks : EVEH
C-
C                                      Author: M. Talby    19-Oct-1989
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
C --
C   If there's any uncertainty in the LEP energy, set it to 91.1 GeV
C   as this is safer.
C --
        IF (ENLEP.LT.80..OR.ENLEP.GT.200.) ENLEP = 91.1
        IF (ENLEP.GT.100..AND.ENLEP.LT.120.) ENLEP = 91.1
      ENDIF
C --
C   Get the number of good tracks NGDTR and the sum of their energy
C   ECHRG
C --
      CALL TRKHAD(NGDTR,ECHRG)
      FECHRG = ECHRG/ENLEP
      IF(NGDTR.GE.5 .AND. FECHRG.GT.0.1) TRKHOK = .TRUE.
C --
999   RETURN
      END
#endif
