      REAL FUNCTION HPHMOD(PHITRK)
C.*******************************************************
CKEY HCALDES HCAL BARREL COORDINATES PHI ANGLE / USER
C.*                                   011186 G.Catanesi *
C! To evaluate the phi value in the barrel module coordinates
C.*
C.*******************************************************
#ifndef DOC
#include "alcons.h"
C.
      IF(MOD(PHITRK,PIBY6) .LE. PIBY12)THEN
         HPHMOD = MOD(PHITRK,PIBY6)
      ELSE
         HPHMOD = PIBY6 - MOD(PHITRK,PIBY6)
      ENDIF
      IF(HPHMOD.GT.PIBY12)HPHMOD = HPHMOD - PIBY12
      RETURN
      END
#endif
