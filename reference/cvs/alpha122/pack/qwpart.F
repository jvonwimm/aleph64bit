      SUBROUTINE QWPART (IPC,COPT)
CKEY WRITE PART /USER
C----------------------------------------------------------------------
C! print row I in bank QPAR
C                                                   H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
      SAVE INIT
#include "qcde.h"
      CHARACTER *(*) COPT
      CHARACTER * 1 C1
      CHARACTER * 8 CFLG(4)
      DATA INIT /0/
#include "qmacro.h"
C----------------------------------------------------------------------
C
      LUSE = 0
      IF (INDEX (COPT,'U') .NE. 0)  LUSE = 1
C
      C1 = ' '
      LHEAD = 0
      IF (INDEX (COPT,'0') .NE. 0)  THEN
        C1 = '0'
        LHEAD = 1
      ENDIF
      IF (INDEX (COPT,'1') .NE. 0)  THEN
        C1 = '1'
        LHEAD = 1
      ENDIF
      IF (INDEX (COPT,'H') .NE. 0)  LHEAD = 1
C
      I1 = IPC
      I2 = IPC
      IF (IPC .EQ. 0)  THEN
        I1 = 1
        I2 = KNQPAR
        CALL QWMESS ('0_QWPART_ Particle listing')
      ENDIF
C
      KOQTRA = IW(NAQTRA) + LMHLEN
      KOQPBT = IW(NAQPBT) + LMHLEN
      KOQPAR = IW(NAQPAR) + LMHLEN - KCQPAR
C
      IH = 20
      DO 100 I=I1,I2
C
        DO 10 IEXT=1,IW(KOQTRA)
   10   IF (IW(KOQTRA+IEXT) .EQ. I)  GO TO 20
        IEXT = 0
C
   20   LBIT = IW(KOQPBT+I)
        IF (LUSE .NE. 0 .AND. LBIT .EQ. 0 .AND. IEXT .EQ. 0)
     &      GO TO 100
        IF (LHEAD .NE. 0 .AND. IH .EQ. 20)  THEN
          WRITE (KUPRNT,1001)  C1
          IH = 0
          C1 = ' '
        ENDIF
        IH = IH + 1
C
        NFLG = 0
        IF (IAND (LBIT, KBIT(1)) .EQ. 0)  THEN
          NFLG = 1
          CFLG(1) = 'notused'
        ENDIF
        IF (IAND (LBIT, KBIT(4)) .NE. 0)  THEN
          NFLG = 1
          CFLG(1) = 'pseudo-p'
        ENDIF
        IF (IAND(LBIT, KBIT(2)) .NE. 0)  THEN
          NFLG = NFLG + 1
          CFLG(NFLG) = 'datacard'
        ENDIF
        IF (IAND(LBIT, KBIT(3)) .NE. 0)  THEN
          NFLG = NFLG + 1
          CFLG(NFLG) = 'MCtable'
        ENDIF
        IF (IEXT .NE. 0)  THEN
          NFLG = NFLG + 1
          CFLG(NFLG) = 'MCevent'
        ENDIF
C
        WRITE (KUPRNT,1002)  I,CQPART(I),KCANTI(I,1),
     &    CQPART(KCANTI(I,1)),QCMASS(I),QCCHAR(I),QCLIFE(I),QCWIDT(I),
     &    IEXT,(CFLG(J),J=1,NFLG)
        IF (INIT .EQ. 0 .AND. KUPTER .NE. 0)  WRITE (KUPTER,1003)
        INIT = 1
  100 CONTINUE
C
      IF (LHEAD .NE. 0)  WRITE (KUPRNT,1004)
C
 1001 FORMAT (A,4X,
     +  '# CQPART',6X,'KCANTI anti-CQPART  QCMASS',7X,
     +  'QCCHAR  QCLIFE',6X,'QCWIDT',7X,'MC# FLAGS (see below)')
 1002 FORMAT (2(I6,1X,A12),G13.6,F7.1,2G12.4,I6,4(1X,A))
 1003 FORMAT (' _QWPART_ Particle listing written to log file')
 1004 FORMAT
     &   (' FLAGS : notused  = particle is not used in the program'/
     &  9X,'pseudo-p = used as pseudo particle'/
     &  9X,'datacard = particle attributes from data cards'/
     &  9X,'MCtable  = particle attributes from MC table'/
     &  9X,'MCevent  = particle appears in MC events')
      END
#endif
