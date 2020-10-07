      SUBROUTINE QKINKV(KFKIV,KLKIV)
CKEY USER VERTEX
C----------------------------------------------------------------------
C!  - Gets vertex numbers for kinks for new tracking
C!
C!   Author   :- J. Boucrot                    21-Apr-1997
C!   Output arguments
C!     KFKIV,KLKIV : first and last kink vertex number
C?
C!======================================================================
#ifndef DOC
#include "qcde.h"
      PARAMETER ( ITYVR = 5 )
#include "qmacro.h"
C----------------------------------------------------------------------
      KFKIV=KFFRV
      KLKIV=KFFRV-1
      KMIN=100000
      KMAX=-1
      IOK=0
      DO 100 IVV=KFREV,KLREV
         IF (KVTYPE(IVV).NE.ITYVR) GO TO 100
         IOK=IOK+1
         KMIN=MIN0(KMIN,IVV)
         KMAX=MAX0(KMAX,IVV)
 100  CONTINUE
      IF (IOK.EQ.0) GO TO 999
      KFKIV=KMIN
      KLKIV=KMAX
C
  999 RETURN
      END
#endif
