      SUBROUTINE KPYZET(ZB)
C------------------------------------------------------------------
C! Transfer the content of fragmention information  to ZB array
C    B.Bloch-Devaux January   1998 for Pythia 6.1
C     comdecks referenced : PYT6COM
C----------------------------------------------------------------------
#ifndef DOC
#include "pyt6com.h"
      REAL*4 ZB(LJNPAR)
      IF (MSTU(90).GT.0) THEN
         DO 10 I = 1,MSTU(90)
         J = MSTU(91+I-1)
 10      ZB(J) = PARU(91+I-1)
      ENDIF
      RETURN
      END
#endif
