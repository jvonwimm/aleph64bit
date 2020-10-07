      SUBROUTINE QWMESE (TEXT)
CKEY WRITE /USER
C----------------------------------------------------------------------
C! print message and run/event number.
C called from : anywhere
C                                                   H.Albrecht 20.09.88
C                                         Modified: E. Blucher 31.08.89
C----------------------------------------------------------------------
#ifndef DOC
C
#include "qcdesh.h"
      CHARACTER *(*) TEXT
      CHARACTER * 1  C1
C----------------------------------------------------------------------
      C1 = ' '
      N = 1
      L = LEN (TEXT)
      IF (TEXT(1:1) .EQ. '0' .OR. TEXT(1:1) .EQ. '1')  THEN
        N = 2
        C1 = TEXT(1:1)
      ENDIF
C
      IF(L.EQ.1)THEN
        WRITE (KUPRNT,1000)  C1,KNEVT,KRUN,KEVT
      ELSE
        WRITE (KUPRNT,1001)  C1,TEXT(N:),KNEVT,KRUN,KEVT
      ENDIF
      IF (KUPTER .EQ. 0)  GO TO 90
      IF(L.EQ.1)THEN
      IF(C1.NE.'1') WRITE (KUPTER,1000)  C1,KNEVT,KRUN,KEVT
      ELSE
        IF (C1 .NE. '1')  THEN
          WRITE (KUPTER,1001)  C1,TEXT(N:),KNEVT,KRUN,KEVT
        ELSE
          WRITE (KUPTER,1002)  TEXT(2:),KNEVT,KRUN,KEVT
        ENDIF
      ENDIF
C
 1000 FORMAT (A/' KNEVT :',I7,', KRUN :',I7,', KEVT :',I7)
 1001 FORMAT (2A/' KNEVT :',I7,', KRUN :',I7,', KEVT :',I7)
 1002 FORMAT ('0',33('----')//1X,A/' KNEVT :',I7,', KRUN :',I7,
     &        ', KEVT :',I7)
   90 END
#endif
