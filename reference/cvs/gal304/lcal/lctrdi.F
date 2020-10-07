*DK lctrdi
      SUBROUTINE LCTRDI
C--------------------------------------------------------------
C! Convert trigger signals to MeV
C. - J.Dines Hansen & P.Hansen - 860417
C. - Modified P.Hansen - 950202
C. - Modifies bank LTTR, fills bank LWTR
C. - Called by  LCDIGI                           from this .HLB
C. - Calls LCWTRI                                from this .HLB
C -----------------------------------------------
#ifndef DOC
#include "jqcom.h"
#include "lcnamc.h"
#include "lccomc.h"
#include "bmacro.h"
C -------------------------------------------------------------
      KINDX = IW(NALTTR)
      IF(KINDX .LE. 0)                   GOTO 999
      DO 120 ISEG = 1,LROWS(KINDX)
        KLL = KROW(KINDX,ISEG)
        DO 110 IST = 1,3
C? Convert bank LTTR to MeV
          IW(KLL+IST) = NINT(IW(KLL+IST)*CHTOE(IST))
  110   CONTINUE
  120 CONTINUE
C
C - Fill Wire Trigger bank 'LWTR'
      CALL LCWTRI
  999 RETURN
      END
#endif
