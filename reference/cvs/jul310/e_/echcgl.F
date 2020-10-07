      SUBROUTINE ECHCGL
C***********************************************************************
C! PERFORM EC HC GLOBAL ANALYSIS
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "rlunit.h"
#include "rcurnt.h"
#include "rflags.h"
#include "rparac.h"
#include "crunp.h"
#include "hdpcut.h"
#include "hspcmd.h"
      LOGICAL EXRIC
      DATA EXRIC/.FALSE./
      IF(JCALPR.EQ.0)GO TO 999
C
C?   Check that necessary banks are present
C
      KECOB = NLINK('ECOB',0)
      KHCLU = NLINK('HCLU',0)
      KETP1 = NLINK('ETP1',0)
      KECT1 = NLINK('ECT1',0)
      KETC2 = NLINK('ETC2',0)
      KERL3 = NLINK('ERL3',0)
      IF (KECOB.EQ.0.OR.KHCLU.EQ.0.OR.
     *    KETP1.EQ.0.OR.KECT1.EQ.0.OR.KETC2.EQ.0.OR.KERL3.EQ.0) GOTO 998
C
      CALL EPRSTO(IER)
      IF(IER.NE.0)GO TO 998
C
      IF (JCALPR.EQ.1)CALL CFPASS(IER)
      IF(IER.NE.0)GO TO 998
      IF(JCALPR.EQ.4) CALL HDSPRE
      IF (JCALPR.GE.2)CALL CASHET(IER)
      IF(IER.NE.0)GO TO 998
      IF (JCALPR.EQ.3)CALL CVRIFY
      IF(JCALPR.GE.2)CALL CRCALO(IER)
      IF(JCALPR.EQ.4.AND.KISPHD.EQ.2) THEN
         IF(EXRIC.OR.JHISRF(JULHC).GE.1) THEN
            CALL CEXCOB(NSPATT)
            CALL CEXHIS
         ENDIF
      CALL CRCHRL(NSPATT)
      ENDIF
      IF(JCALPR.EQ.4) CALL CRCPAR
  999 RETURN
  998 CONTINUE
      CALL RERROR('ECHCGL',1,' Error from EPRSTO,CFPASS or CASHET')
      RETURN
      END
#endif
