      INTEGER FUNCTION HCLATU(IPOR,IPL)
C------------------------------------------------
CKEY HCALDES HCAL TUBE PLANE / USER
C!  Compute the last tube of a plane
C!
C!      Author       : G.Catanesi 10/01/87
C!
C!      IPOR/I       : Portion number
C!      IPL /I       : Plane number
C!
C!
C-----------------------------------------------
#ifndef DOC
#include "hcgeom.h"
C
      IF(IPOR.EQ.LPBAR)THEN
         HCLATU=NEITHC(IPL)*NHCEIT
      ELSE
         IF(IPL.LE.NHCINL)THEN
            HCLATU=NHCIND*NHCEIT*2
         ELSE
            HCLATU=NHCOUD*NHCEIT*2
         ENDIF
      ENDIF
      END
#endif
