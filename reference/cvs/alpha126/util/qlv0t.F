      SUBROUTINE QLV0T(KFLVT,KLLVT)
CKEY USER VERTEX
C----------------------------------------------------------------------
C!  - Gets ALPHA "track" limits for Long V0s for new tracking
C!
C!   Author   :- J. Boucrot                    21-Apr-1997
C!   Output arguments
C!     KFLVT,KLLVT : first and last Long V0 "track" number
C?
C!======================================================================
#ifndef DOC
#include "qcde.h"
      PARAMETER ( ITYVR = 3 )
#include "qmacro.h"
C----------------------------------------------------------------------
      KFLVT=KFFRT
      KLLVT=KFFRT-1
      KMIN=100000
      KMAX=-1
      IOK=0
      DO 100 IVV=KFREV,KLREV
         IF (KVTYPE(IVV).NE.ITYVR) GO TO 100
         ITK=KVINCP(IVV)
         IF (ITK.LE.0) GO TO 100
         IOK=IOK+1
         KMIN=MIN0(KMIN,ITK)
         KMAX=MAX0(KMAX,ITK)
 100  CONTINUE
      IF (IOK.EQ.0) GO TO 999
      KFLVT=KMIN
      KLLVT=KMAX
C
  999 RETURN
      END
#endif
