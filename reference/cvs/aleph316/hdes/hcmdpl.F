               SUBROUTINE HCMDPL(IPOR,IMOD,ILAY,COIN,COUT)

C----------------------------------------------------------
CKEY HCALDES HCAL TRANSFORM COORDINATES / USER
C
C! Trasform coordinates from Module R.S. to Plane R.S.
C!
C!                            Author:G.Catanesi 88/5/10
C!
C!     Input:
C!              IPOR/I portion number
C!              IMOD/I module number
C!              ILAY/I layer number
C!              COIN(3)/R coordinates in Module R.S.
C!
C!     Output:
C!              COUNT(2)/R coordinates in Plane R.S.
C!
C!---------------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
               DIMENSION COIN(*),COUT(*)
C
C-------------------------------------------------------------
C
              IF(IPOR.EQ.LPBAR) THEN
                    COUT(1) = COIN(1) + HCLAWI(ILAY)/2.
                    COUT(2) = COIN(3)
              ELSE
                    CALL HYTRSF(COIN(1),COIN(2),IPOR,IMOD)
                    COUT(1) = COIN(1)
                    COUT(2) = COIN(2)
              ENDIF
C
              RETURN
              END
C
#endif
