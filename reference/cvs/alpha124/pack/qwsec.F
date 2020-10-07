      SUBROUTINE QWSEC (NSEC,CFL)
CKEY WRITE /INTERNAL
C----------------------------------------------------------------------
C! print section NSEC
C called from user routines
C                                                   H.Albrecht 20.09.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      CHARACTER * (*) CFL
      CHARACTER * 10  C9,C10
#include "qmacro.h"
C----------------------------------------------------------------------
      IF (NSEC .LE. 0 .OR. NSEC .GT. 14)  THEN
        CALL QWMESE ('_QWSEC_ Called with wrong section number')
        GO TO 90
      ENDIF
      IF (KLOCUS(3,NSEC) .EQ. 0)  GO TO 90
C
      N = 0
      C9 = CFL
      C10 = 'H' // C9
      DO 10 I=KLOCUS(1,NSEC),KLOCUS(2,NSEC)
        IF (NSEC .LT. KSREV)  THEN
          IF (XLOCK(I))  GO TO 10
          CALL QWITK (I,C10)
        ELSE
          CALL QWIVX (I,C10)
        ENDIF
        C10 = ' '
        N = N + 1
        IF (N .EQ. 20)  THEN
          N = 0
          C10 = 'H'
        ENDIF
   10 CONTINUE
C
   90 END
#endif
