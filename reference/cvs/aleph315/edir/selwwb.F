        SUBROUTINE SELWWB(TRKHOK)
C----------------------------------------------------------------------
CKEY EDIR GOOD TRACKS
C! Select low multiplicity events for WW to lnulnu decay mode.
C! Good event = number of good tracks <=7 + E(track)/ELEP > 0.05.
C!              Also, 0 positive or 0 negative tracks.     
C-
C   Input  : none
C   Output : TRKHOK is true if good event for WW B selection
C-
C   Called by   : SELEVT
C   Calls  : TRKWWS
C   Input banks : EVEH
C-
C                                      Author: J. Yamartino 28-May-1996
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "evehjj.h"
C --
      INTEGER NGDTP,NGDTN
      LOGICAL TRKHOK
      EXTERNAL NAMIND, ALELEP
      DATA LRUN/0/
      DATA NAEVEH / 0 /
C --
      TRKHOK = .FALSE.
C --
      IF( NAEVEH .EQ. 0 ) NAEVEH = NAMIND('EVEH')
      KEVEH = IW(NAEVEH)
      IF(KEVEH.LE.0) GOTO 999
C --
C   Get LEP energy for run number NRUN
C   If not found, set it to the lowest relevant energy
C --
      NRUN = IW(KEVEH+JEVERN)
      NEVT = IW(KEVEH+JEVEEV)
      IF(NRUN.NE.LRUN) THEN
        LRUN = NRUN
        ENLEP = ALELEP(NRUN)
        IF(ENLEP.LT.80.) ENLEP = 130.0
      ENDIF
C --
C   Get the number of good tracks NGDTR and the sum of their energy
C   ECHRG
C --
      CALL TRKWWS(NGDTP,NGDTN,ECHRG)
      FECHRG = ECHRG/ENLEP
      IF((NGDTP.EQ.0.OR.NGDTN.EQ.0).AND.NGDTP+NGDTN.LE.7
     +             .AND.FECHRG.GT.0.05) TRKHOK = .TRUE.
C --
999   RETURN
      END
#endif
