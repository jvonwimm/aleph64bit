      SUBROUTINE QWHEAD (COPT,CTXT)
CKEY HEADER /USER
C----------------------------------------------------------------------
C! print event header
C called from user routines
C                                                   H.Albrecht 20.09.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER * (*) COPT,CTXT
C----------------------------------------------------------------------
C
      IF (KFFILL .EQ. 0)  CALL QFHEAD
C
      LHEAD = 0
      IF (INDEX (COPT,'0') .NE. 0)  THEN
        CALL QWMESS (' ')
        LHEAD = 1
      ENDIF
      IF (INDEX (COPT,'1') .NE. 0)  THEN
        CALL QWMESS ('1')
        LHEAD = 1
      ENDIF
      IF (INDEX (COPT,'H') .NE. 0)  LHEAD = 1
      IF (CTXT .NE. ' ')  CALL QWMESS (CTXT)
C
      LU = KUPRNT
      DO 100 NU=1,2
        IF (LU .EQ. 0)  GO TO 100
C
        IF (LHEAD .NE. 0)  WRITE (LU,1001)
        WRITE (LU,1002)  KNEVT,KEXP,KRUN,KEVT,QELEP,KEVERT,KEVETY
  100 LU = KUPTER
C
 1001 FORMAT ('  KNEVT   KEXP   KRUN   KEVT   QELEP KEVERT KEVETY')
 1002 FORMAT (I7,3I7,F8.3,2I7)
      END
#endif
