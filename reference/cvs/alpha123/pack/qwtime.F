      SUBROUTINE QWTIME
CKEY WRITE TIME /USER
C----------------------------------------------------------------------
C!  print time consumption
C                                                   H.Albrecht 06.11.88
C                          Modified for PVM on SAGA  T. Oest  26.07.95
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C----------------------------------------------------------------------
C
      CALL TIMEL (QTIMEL)
      CALL TIMEX (TITO)
      IF (QTIMET .EQ. 0.)  QTIMET = TITO
      IF (QTIMEI .EQ. 0.)  QTIMEI = TITO
      TITE = TITO - QTIMET
      TITO = TITO - QTIMES
      TIPR = QTIMET - QTIMEI
      TIIN = QTIMEI - QTIMES
      TIAV = 0.
      IF (KNEVT .NE. 0)  TIAV = TIPR / KNEVT
C
      LU = KUPRNT
   10 DO 100 NU=1,2
        IF (LU .EQ. 0)  GO TO 100
        WRITE (LU,1001)  KNEVT,TITO,TIIN,TIPR,TIAV,TITE,QTIME,
     +                   QTIMEL
  100 LU = KUPTER
C
 1001 FORMAT ('0_QWTIME_ : total     init',
     +   I9,8X,'1     term     TIME     time'/13X,
     +   'time     time   events    event     time     card     left'/
     +   8X,3F9.2,F9.3,3F9.2)
#if defined(UNIX)
C
      CALL QPVMTI(KUPRNT,KNEVT,TITO,TIIN,TIPR,TIAV,TITE)
C
#endif
      END
#endif
