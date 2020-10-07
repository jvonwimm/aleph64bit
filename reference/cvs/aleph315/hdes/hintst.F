        LOGICAL FUNCTION HINTST(INTET,INPHI,KS)
C------------------------------------------------------------------
CKEY HCALDES HCAL STATUS TOWER / USER
C! test if storey indices are in correct range
C!
C!      G.Capon               date : 861001
C!      G.Catanesi 25-nov-1988 (mod. for D.B. access
C!
C------------------------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
C
      HINTST=.TRUE.
C
      IF (INTET.LT.1 .OR. INTET.GT.LHCTR
     &               .OR. KS.LT.1 .OR. KS.GT.2) THEN
         HINTST=.FALSE.
      ELSE
         NPHI=12*(2**IHCREG(INTET))
         IF (INPHI.LT.1 .OR. INPHI.GT.NPHI) HINTST=.FALSE.
      ENDIF
C
      END
#endif
