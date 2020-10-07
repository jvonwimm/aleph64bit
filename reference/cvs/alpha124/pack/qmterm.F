      SUBROUTINE QMTERM (TEXT)
CALPHA TERM /INTERNAL
C----------------------------------------------------------------------
C! system termination
C called from anywhere
C calls : QUTERM (user routine)
C calls : QUTHIS (histogram output)
C                                                   H.Albrecht 20.09.88
C                                   Modified:       E.Blucher  17.04.90
C                                   Modified:       G.Graefe   10.02.95
C                          Modified for PVM on SAGA  T. Oest  26.07.95
C----------------------------------------------------------------------
#ifndef DOC
C
#include "minprd.h"
#include "qcdesh.h"
C
      CHARACTER CFORM*3
      CHARACTER *(*) TEXT
C----------------------------------------------------------------------
      CALL QWMESS (TEXT)
      CALL QWMESS ('_QMTERM_ --- program termination ----------')
#if defined(UNIX)
C_PVM
C
C  PVM: send EOF message
C
      Call QPVMEN
C_PVM
#endif
C
C       protection against recursive calls :
C
      IF (KSTATU .GT. 0)  RETURN
      KSTATU = 1
      CALL TIMEX (QTIMET)
C
C       last event number
C
      IF (KNEVT .NE. 0)
     +    CALL QWMESE ('_QMTERM_ Last event :')
C
C       syntax flag ?
C
      IF (XSYNTX)  THEN
        CALL QWMESS ('_QMTERM_ Syntax flag --> QUTERM not called')
        GO TO 10
      ENDIF
C
C       copy job ?
C
      IF (XCOPYJ)  GO TO 10
C
C       last run
C
      IRNEW=0
      IROLD=KRUN
      CALL QMNEWR (IROLD,IRNEW)
C
C       user termination
C
      CALL QWMESS ('_QMTERM_ Call QUTERM')
      IF (IMIPRD.EQ.1) THEN
         CALL MQUTERM
         GO TO 999
      ENDIF
      CALL QUTERM
      IF (KDEBUG .GT. 0)  CALL QWPARS
C
C       Statistics of NDSTP if output is a NanoDST
C
      IF (XWNANO) CALL QNTERM
C
C       histogram output
C
      CALL QUTHIS
C
C       BOS statistics
C
   10 IF (KDEBUG .GT. 0)  CALL BOSTA
C
C       output event buffers
C
      IF (KNREOU.GT.0) THEN
         CALL ABWEND
      ENDIF
C
C       statistics
C
C
C-    Summary lepton selection
C
      CALL QLTERM
C
      CALL QWFILE
      CALL QWSUMM
      CALL QTIMEZ
      CALL QWTIME
#if defined(UNIX)
C_PVM
C
C merge output files ( PVM )
C dispose after hist files are merged
C
      CALL QPVMTE(KUPRNT,XHISTO,CQFHIS)
C_PVM
#endif
      CALL ACLOSE (0, IER)
      CALL QWMESS ('_QMTERM_ Stop.')
#if defined(UNIX)
C_PVM
C  pvm termination
      CALL QPVMEX(KUPRNT)
C_PVM
#endif
 999  CONTINUE 
      STOP
      END
#endif
