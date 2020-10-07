*DK lcwire
      SUBROUTINE LCWIRE(MODU,LAY,NHIT)
C--------------------------------------------------------------
C! Deposit energy in wire planes
C. - J.Dines Hansen & P.Hansen - 860417
C. - Fills bank LWHT
C. - Input : MODU   = Module No.             [1,4]
C. -         LAY    = Layer No.         [1,LCNWPL]
C. -         NHIT   = Number of hits in this layer
C. - Called by  LCSHOW,LCTRAK                    from this .HLB
C -----------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "lcnamc.h"
#include "lccomc.h"
#include "bmacro.h"
C ------------------------------------------------------------
      KINDX = IW(NALWHT)
      IF(KINDX .LE. 0)                              GOTO 999
      IF (LAY .LE. 0 .OR. LAY .GT. LCNWPL)          GOTO 999
      KWH = KROW(KINDX,MODU)
      IW(KWH+1) = MODU
      IW(KWH+1+LAY) = IW(KWH+1+LAY) + NHIT
  999 RETURN
      END
#endif
