      SUBROUTINE ESWEH (ETOT)
C----------------------------------------------------------------------
CKEY EDIR EVENT ENERGY
C! Calculate Energy of event.
C-
C   Input  : None
C   Output : ETOT  = Total Ecal energy of the event
C-
C   Called by   : SELCAL
C   Calls  : ECPHCO
C   Input banks : EVEH,PHCO
C-
C                          Authors: M.N. Minard + M. Pepe     15/09/89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "phcojj.h"
#include "evehjj.h"
C --
      COMMON / EWIR / EWIRE ( 36 )
      DATA LRUN / 0 /
C --
#include "bmacro.h"
C --
C   Calculate Lep energy
C --
      ETOT = 0.
      KEVEH = IW(NAMIND('EVEH'))
      IF (KEVEH.LE.0) THEN
         GO TO 999
      ENDIF
      NRUN = IW(KEVEH+JEVERN)
      NEVT = IW(KEVEH+JEVEEV)
      IF (NRUN.NE.LRUN) THEN
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
C   Calculate E-From Ecal wires
C --
      EWECAL = 0
      DO 10 IECAL = 1, 36
      EWECAL = EWECAL + EWIRE (IECAL)
 10   CONTINUE
C --
C   Calculate Hcal Energy
C --
      EMATCH = 0
      NAPHCO = NAMIND('PHCO')
      KPHCO = IW (NAPHCO)
      NPHCO = 0
      IF (KPHCO.GT.0) NPHCO = LROWS(KPHCO)
      EPHCO = 0.
      DO 30 IPHCO = 1,NPHCO
C --
C   Add up Hcal tower energy which matches the digital information
C --
      CALL ECPHCO(IPHCO,IOK)
C --
C   fix up due to absence of digital readout in hcal endcaps
C --
      IF(IOK.GT.0) EMATCH = EMATCH + RTABL(KPHCO,IPHCO,JPHCEC)
      EPHCO = EPHCO + RTABL ( KPHCO,IPHCO ,JPHCER)
 30   CONTINUE
C --
C   Now calculate sum from differents contributions
C --
      ETOT =(EWECAL + EMATCH)/ENLEP
  999 RETURN
      END
#endif
