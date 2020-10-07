        INTEGER FUNCTION HNREG(ITETA)
C-----------------------------------------------------------------
CKEY HCALDES HCAL GEOM TOWER THETA REGION / USER
C
C! computes Hcal region # from theta index
C!
C!   G.Capon 10-jun-1987
C!   G.Catanesi 25-nov-1988 (mod. for D.B. access
C
C-----------------------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
C
        JTETA = ITETA
        IF (JTETA.GT.LHCTR/2) JTETA=LHCTR+1-JTETA
        HNREG=2
        IF (JTETA.LE.IHCTID(1)) HNREG=1
        IF (JTETA.GT.IHCTID(2)) HNREG=3
        RETURN
        END
#endif
