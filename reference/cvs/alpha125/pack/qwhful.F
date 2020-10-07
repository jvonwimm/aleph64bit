      SUBROUTINE QWHFUL (COPT,CTXT)
CKEY HEADER /USER
C----------------------------------------------------------------------
C! print full event header
C called from user routines
C                                                   H.Albrecht 20.09.88
C                                        Modified:  E.Lancon   19.07.89
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
     +                  ,KEVEDA,KEVETI
        IF (LHEAD .NE. 0)  WRITE (LU,1003)
        WRITE (LU,1004)  KDHENX,KDHENP,KDHENM,KDHENV,KDHENJ
        IF (LHEAD .NE. 0)  WRITE (LU,1005)
        WRITE (LU,1006)  QDHEEC,QDHEEL,QDHEPF,QDHETH,QDHEPH,QDHEEF
     +                  ,QDHEET
        IF (LHEAD .NE. 0)  WRITE (LU,1007)
        WRITE (LU,1008)  QDHET1,QDHEP1,QDHET2,QDHEP2,QDHEE1,QDHEE2
     +                  ,QDHEE3
  100 LU = KUPTER
C
 1001 FORMAT (/,80('_'),/
     +         ,T7,'KNEVT',T16,'KEXP',T24,'KRUN',T32,'KEVT'
     +        ,T39,'QELEP',T46,'KEVERT',T54,'KEVETY'
     +        ,T62,'KEVEDA',T70,'KEVETI')
 1002 FORMAT (3X,4I8,F8.3,4I8)
 1003 FORMAT (/,T6,'KDHENX',T14,'KDHENP',T22,'KDHENM',T30,'KDHENV',
     +         T38,'KDHENJ')
 1004 FORMAT (3X,5I8)
 1005 FORMAT (/,T6,'QDHEEC',T14,'QDHEEL',T22,'QDHEPF',T30,'QDHETH',
     +         T38,'KDHEPH',T46,'QDHEEF',T54,'QDHEET')
 1006 FORMAT (3X,7F8.3)
 1007 FORMAT (/,T6,'QDHET1',T14,'QDHEP1',T22,'QDHET2',T30,'QDHEP2',
     +         T38,'KDHEE1',T46,'QDHEE2',T54,'QDHEE3')
 1008 FORMAT (3X,7F8.3)
      END
#endif
