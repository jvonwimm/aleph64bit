      INTEGER FUNCTION KCHT(ITK)
CKEY TRACK /USER
C----------------------------------------------------------------------
C! Get ALPHA track number of original copy of a charged track.
C!                                                  E. Blucher 24.1.92
C----------------------------------------------------------------------
#ifndef DOC
      PARAMETER ( MAXTR = 20 )
#include "qcde.h"
#include "qmacro.h"
      KCHT=0
      NTR=0
      ITKSAM=KSAME(ITK)
 10   IF (ITKSAM.GE.KFCHT.AND.ITKSAM.LE.KLCHT) THEN
        KCHT=ITKSAM
        GO TO 90
      ENDIF
      IF (ITKSAM.EQ.ITK) GO TO 90
      ITKSAM=KSAME(ITKSAM)
      NTR=NTR+1
      IF (NTR.GE.MAXTR) THEN
         KCHT=ITKSAM
         GO TO 90
      ENDIF   
      GO TO 10
  90  CONTINUE
      END
#endif
