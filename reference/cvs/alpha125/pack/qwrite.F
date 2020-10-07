      SUBROUTINE QWRITE
CKEY WRITE EVENT /USER
C----------------------------------------------------------------------
C! event output routine
C                                         Authors: H.Albrecht 08.11.88
C                                        Modified: E. Blucher 11.05.89
C                          Modified for Event Dir. E. Blucher 16.09.89
C                             Modified for NanoDST G. Graefe  10.02.95
C                          Modified for PVM on SAGA  T. Oest  26.07.95
C----------------------------------------------------------------------
#ifndef DOC
      SAVE NERR
#include "qcdesh.h"
      INTEGER BKINCA
      LOGICAL BTEST
      DATA NERR /0/
C----------------------------------------------------------------------
C
      IF (KNEOUT .GE. KNWRLM)  GO TO 90
#if defined(UNIX)
C_PVM
C
C     CALL WRITE ROUTINE FOR PVM
C
      CALL QPVMWR(*90)
C_PVM
#endif
      IF(XWMINI)THEN
        CALL MINDST(CQELST,CQRLST)
      ENDIF
C
      IF (XWNANO) THEN
        CALL QNEVNT(QT,KT,QV,KV)
      ENDIF
C
C       write run record for FILO option SELRUN
C
      IF (XWRRUN)  CALL QWRUNR
C
C       write event record
C
      IF (XWREVT)  GO TO 90
      XWREVT = .TRUE.
C
C++   If we are writing Mini-DST, swop the E list for the Mini list.
C
      IF(XWMINI.OR.XWNANO) THEN
        CALL BLIST(IW,'S+','E')
        CALL BLIST(IW,'E=',CQELST)
      ENDIF
      CALL ABWSEL ('E')
      IF (KEVT .NE. 0)  THEN
        KNEOUT = KNEOUT + 1
        KLEOUT = KEVT
        KLROUT = KRUN
      ENDIF
      KNREOU = KNREOU + 1
      IF (KNEOUT .EQ. 1.AND.KEVT.NE.0)  THEN
        IF (KFEOUT .LT. 0)  CALL QWMESS
     +    ('0_QWRITE_ No FILO card given: NO OUTPUT!')
        CALL QWMESE ('_QWRITE_ First event on output file')
      ENDIF
C
   90 END
#endif
