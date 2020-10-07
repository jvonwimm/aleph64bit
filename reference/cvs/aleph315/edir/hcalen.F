      SUBROUTINE HCALEN(EHCAL)
C----------------------------------------------------------------------
CKEY EDIR HCAL PADS ENERGY
C! HCAL total pads energy.
C-
C   Input  : None
C   Output : EHCAL : Hcal total pads energy
C-
C   Called by   : ECALSL
C   Calls  : None
C   Input banks : PHCO
C-
C                                        Author: M. Talby September 89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "phcojj.h"
#include "bmacro.h"
C --
      EHCAL = 0.
C --
      KPHCO=IW(NAMIND('PHCO'))
      IF(KPHCO.LE.0) GOTO 999
C --
C   Total Hcal pads energy
C --
      NPHCO = LROWS(KPHCO)
      DO 30 NHC = 1,NPHCO
        EHCAL = EHCAL + RTABL(KPHCO,NHC,JPHCEC)
   30 CONTINUE
C --
  999 RETURN
      END
#endif
