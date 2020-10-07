            FUNCTION HCDPSP(IPOR,ILAY,INUM)
C-------------------------------------------------------
C
CKEY HCALDES HCAL BARREL SPACER /USER
C
C! Return the width of the spacer INUM in the plane ILAY
C!         of the portion IPOR
C!
C!                                  Author:G.Catanesi 880729
C!
C!    input:
C!           IPOR/I  portion number
C!           ILAY/I  layer number
C!           INUM/I  spacer number in layer ILAY
C!
C!-------------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
C
            HCDPSP = 0.
C
            IF(IPOR.EQ.LPBAR)THEN
               HCDPSP = HCSPLT(ILAY,INUM)
            ELSE
               HCDPSP = HCAPSL
            ENDIF
C
            RETURN
            END
#endif
