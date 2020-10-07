      SUBROUTINE QFMCVX
CKEY FILL MC VERTEX/INTERNAL
C-----------------------------------------------------------------------
C! Fill MC vertices
C!                                                  H.Albrecht 18.10.88
C!called from QFILMC
C-----------------------------------------------------------------------
#ifndef DOC
#include "fverjj.h"
#include "qcdesh.h"
C-----------------------------------------------------------------------
C
      JFVER = IW(NAFVER) + LMHLEN
      LFVER = IW(JFVER-1)
C
C MC Vertex section in QVRT :
C
      KNMCV = IW(JFVER)
      KLMCV = KLMCV + KNMCV
      IF (KLMCV .GE. KLFRV)  CALL QSBANK ('QVRT', KLMCV + 50)
      JQVRT = KOQVRT + KFMCV * KCQVRT
      KFFRV = KLMCV + 1
C
      DO 10 IVR = 1, KNMCV
C
C Vertex position
        RW(JQVRT+JQVRVX) = RW(JFVER+JFVEVX)
        RW(JQVRT+JQVRVY) = RW(JFVER+JFVEVY)
        RW(JQVRT+JQVRVZ) = RW(JFVER+JFVEVZ)
C
        IW(JQVRT+JQVRAF) = JFVER
        IW(JQVRT+JQVRVN) = IVR
        IW(JQVRT+JQVRND) = 0
        RW(JQVRT+JQVREM) = -1.
        RW(JQVRT+JQVRET) = -1.
        IW(JQVRT+JQVRIP) = 0
C
C Incoming particle
        IF (IW(JFVER+JFVEIP) .NE. 0)
     +    IW(JQVRT+JQVRIP) = IW(JFVER+JFVEIP) + KLMCT
        JFVER = JFVER + LFVER
   10 JQVRT = JQVRT + KCQVRT
C
      END
#endif
