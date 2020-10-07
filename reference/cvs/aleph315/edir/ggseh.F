      SUBROUTINE GGSEH (ETOT)
C----------------------------------------------------------------------
CKEY EDIR EVENT ENERGY GAMMA_GAMMA
C! Calculate Energy of event.
C-
C   Input  : None
C   Output : ETOT  = Total Ecal + HCAL energy of the event
C-
C   Called by   : SELGGE
C   Calls  : ECPHCO
C   Input banks : EVEH,PHCO
C-
C                          Author: A.Finch 24 May 1996
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "phcojj.h"
#include "evehjj.h"
C --
      EXTERNAL NAMIND
      COMMON / EWIR / EWIRE ( 36 )
      DATA LRUN / 0 /
      DATA NAPHCO / 0 /
C --
#include "bmacro.h"
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
      IF( NAPHCO .EQ. 0 ) NAPHCO = NAMIND('PHCO')
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
      ETOT =(EWECAL + EMATCH)
  999 RETURN
      END
#endif










