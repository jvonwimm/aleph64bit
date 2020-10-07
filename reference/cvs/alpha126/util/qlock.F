      SUBROUTINE QLOCK (ITK)
CKEY LOCK /USER
C----------------------------------------------------------------------
C! lock track ITK.
C
C called from user routines
C                                                   H.Albrecht 27.11.88
C                                         Modified: E.Blucher  01.08.90
C                                         Modified: E.Blucher  03.02.93
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcde.h"
      DATA IERR /0/
#include "qmacro.h"
C----------------------------------------------------------------------
C
      N = IW(KOQVEC+ITK*KCQVEC+JQVECL)
C
      IF (N .EQ. 0)  THEN
        IERR = IERR + 1
        IF (IERR .LT. 10)  CALL QWMESE
     &('_QLOCK_ Particle with unknown origin (RECO/MC) not locked')
        GO TO 90
      ENDIF
      N = KCLARM(N)
C
      DO 10 I=1,KLOCKM
      KLOCK1(I,N) = IOR (KLOCK1(I,N), KBMASK(ITK,I))
      KLOCK0(I,N) = IOR (KLOCK1(I,N), KLOCK2(I,N))
10    CONTINUE
C
   90 END
#endif
