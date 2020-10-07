      SUBROUTINE QKINKT(NKINK,LISKI)
CKEY USER VERTEX
C----------------------------------------------------------------------
C!  - Gets ALPHA "track" list for tracks ending in a Kink vertex
C!
C!   Author   :- J. Boucrot                    21-Apr-1997
C!   Output arguments
C!     NKINK = number of reconstructed charged tracks with a kink
C!     LISKI = list of ALPHA track numbers ( 50 maximum )
C?
C!======================================================================
#ifndef DOC
#include "qcde.h"
      PARAMETER ( ITYVR = 5 , MAXTR = 50)
      INTEGER LISKI(*)
#include "qmacro.h"
C----------------------------------------------------------------------
      NKINK=0
      CALL VZERO(LISKI,MAXTR)
      DO 100 IVV=KFREV,KLREV
         IF (KVTYPE(IVV).NE.ITYVR) GO TO 100
         ITK=KVINCP(IVV)
         IF (ITK.LE.0) GO TO 100
         NKINK=NKINK+1
         IF (NKINK.GT.MAXTR) GO TO 999
         LISKI(NKINK)=ITK
 100  CONTINUE
C
 999  RETURN
      END
#endif
