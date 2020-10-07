          REAL FUNCTION HCDRBM(IPOR)
C----------------------------------------------------------
CKEY HCALDES HCAL GEOM SUBDETECTOR / USER
C
C!  Return the thickness of the module outer wall
C!    in the subdetector IPOR
C!
C!                             Author:G.Catanesi 070188
C!
C!      input:
C!                IPOR/I  subdetector number
C!
C----------------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
C
      HCDRBM = 0.
C
      IF (IPOR.EQ.2)THEN
         HCDRBM = HCDEWI
      ELSE
         HCDRBM = HCDREC
      ENDIF
C
      RETURN
      END
C
#endif
