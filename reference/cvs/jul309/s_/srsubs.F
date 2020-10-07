      LOGICAL FUNCTION SRSUBS(ISIDE,N3TOT,IARR,N)
C----------------------------------------------------------------------
C! Is IARR subset of ex. patch?
C!
C!    Author:     H. Meinhard       13-Jan-1988
C!    Modified:   H. Meinhard       15-Feb-1988
C!
C!    Input:      - ISIDE     /I    current side [1,2]
C!                - N3TOT     /I    number of patches found so far,
C!                                  without new 2-patches [0,*]
C!                - IARRi     /I    addresses of hits in patch cand.
C!                                  [1,*]
C!                - N         /I    number of hits in patch cand. [4,7]
C!    Output:     - SRSUBS    /L    .true. if patch cand. is subset of
C!                                  3-patch
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sanamc.h"
#include "spatjj.h"
#include "spcpjj.h"
#include "ssppjj.h"
      INTEGER IARR(*)
#include "bmacro.h"
C----------------------------------------------------------------------
C locate patch banks
      KSPAT = IW(NASPAT)
      KSPCP = IW(NASPCP)
      KSSPP = IW(NASSPP)
C
      SRSUBS = .FALSE.
C
C loop over 3-patches
      DO 320 IP = ITABL(KSSPP,ISIDE,JSSPPB), N3TOT
        NW = ITABL(KSPAT,IP,JSPANW)
C
C loop over hits of patch candidate
        DO 310 I = 1, N
C
C search for hit in 3-patch
          DO 300 IH = I, MIN(NW,I+3)
            IF (IARR(I) .EQ. ITABL(KSPCP,IP,JSPCPC+IH-1))   GOTO 310
  300     CONTINUE
C
C hit of patch candidate without correspondance to hit of 3-patch.
C try with next 3-patch
          GOTO 320
C
C bottom of loop over hits of patch candidate
  310   CONTINUE
C
C successful!
        SRSUBS = .TRUE.
        GOTO 999
C
C bottom of loop over 3-patches
  320 CONTINUE
C
      GOTO 999
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
#endif
