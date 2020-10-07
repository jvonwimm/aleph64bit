              SUBROUTINE HCDWSP(IPOR,ILAY,INUM,YEDGE)
C---------------------------------------------------
CKEY HCALDES HCAL  BARREL SPACER / USER
C
C! Evaluate the upper position of spacer INUM-1 and the lower position
C! of the spacer INUM in the layer ILAY of portion IPOR.
C! The R.S used is the PLANE R.S.
C!
C!                           Author: G.Catanesi 080188
C!
C!    input:
C!           IPOR/I  portion number
C!           ILAY/I  layer number
C!           INUM/I  spacer number in layer ILAY
C!
C!    output:
C!           YEDGE/R y edges
C!
C!-------------------------------------------------------
C.
#ifndef DOC
#include "hcgeom.h"
             DIMENSION YEDGE(*)
C
             YEDGE(1) = 0.
             YEDGE(2) = 0.
C
             WIDSP = HCDPSP(IPOR,ILAY,INUM)
C
         IF(IPOR.EQ.LPBAR)THEN
               YEDGE(2) = HCBLSP(ILAY,INUM)
            IF(INUM.NE.1)THEN
               YEDGE(1)=HCBLSP(ILAY,INUM-1)+WIDSP
            ENDIF
         ELSE
               YEDGE(2) = HCAPDE(INUM)
            IF(INUM.NE.1)THEN
               YEDGE(1) =HCAPDE(INUM-1)+WIDSP
            ENDIF
         ENDIF
C
         RETURN
         END
#endif
