      SUBROUTINE QTIMEZ
CKEY TIME /USER
C----------------------------------------------------------------------
C! Time consumption monitoring.
C                                          Authors: H. Albrecht 16.1.90
C
C  Called from QMTERM
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      SAVE NC,TI,MC
      PARAMETER (MP = 10)
      DIMENSION NC(MP), TI(MP)
      DATA NC /MP * 0/, TI /MP * 0./, MC /-1/
C----------------------------------------------------------------------
      IF (MC .LE. 0)  GO TO 90
C
      TT = 0.
      DO 10 I=1,MP
 10   TT = TT + TI(I)
      WRITE (KUPRNT, 1001)
      IF (KUPTER .NE. 0)  WRITE (KUPTER, 1001)
C
      DO 20 I=1,MP
        IF (NC(I) .NE. 0)  THEN
          WRITE (KUPRNT, 1002)
     &      I, NC(I), TI(I), TI(I)/NC(I), TI(I)*100/TT
          IF (KUPTER .NE. 0)  WRITE (KUPTER, 1002)
     &      I, NC(I), TI(I), TI(I)/NC(I), TI(I)*100/TT
        ENDIF
 20   CONTINUE
      GO TO 90
C
      ENTRY QTIMED (IP)
C
      CALL TIMED (T)
      MC = MC + 1
      IF (MC .NE. 0 .AND. IP .GT. 0 .AND. IP .LE. MP)  THEN
        NC(IP) = NC(IP) + 1
        TI(IP) = TI(IP) + T
      ENDIF
 90   RETURN
 1001 FORMAT
     & (' QTIMED(II) Ncalls   total_time  time/call     %')
 1002 FORMAT (8X,I2,I8,2G12.3,F6.1)
      END
#endif
