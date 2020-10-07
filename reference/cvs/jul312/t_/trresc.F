      SUBROUTINE TRRESC
C
C----------------------------------------------------------------*
C! Rescue a chain,
C! break it in half and refit
C!
C!  M.Mermikides  6/10/86   V 1.0
C!
C!  Called by TRKFND
C!
C----------------------------------------------------------------*
#ifndef DOC
C
#include "tchain.h"
#include "rparac.h"
#include "rlunit.h"
C
      NC = NHCHAI
      N1 = NC/2
      N2 = NC - N1
      NHCHAI = N1
      CALL TRFTCH
      IF (ISTCHA.EQ.0) THEN
         DO 10 I=1,N2
            IRCHAI(I) = IRCHAI(N1+I)
            IHCHAI(I) = IHCHAI(N1+I)
            D2CHAI(I) = D2CHAI(N1+I)
 10      CONTINUE
         NHCHAI = N2
         CALL TRFTCH
      ENDIF
      RETURN
      END
#endif
