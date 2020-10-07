      SUBROUTINE QWPARS
CKEY WRITE PART /USER
C----------------------------------------------------------------------
C! print particle table summary
C                                                   H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      CHARACTER*4 CHAINT
#include "qmacro.h"
C----------------------------------------------------------------------
C
C       print particle names on MC particle table
C
      CALL QWMESS ('0_QWPARS_ Particle table summary')
      IF (.NOT. XMCEV)  GO TO 200
      LPART = IW(NAPART)
      IF (LPART .EQ. 0)  THEN
        CALL QWMESS ('_QWPARS_ MC particle table does not exist')
        GO TO 200
      ENDIF
      WRITE (KUPRNT,1011) KNPAST
      WRITE(KUPRNT,1012)(I,CHAINT(IW(LPART+(I-1)*KCQPAR+JQPANA+LMHLEN)),
     &  CHAINT(IW(LPART+(I-1)*KCQPAR+JQPANA+1+LMHLEN)),
     &  CHAINT(IW(LPART+(I-1)*KCQPAR+JQPANA+2+LMHLEN)),
     &  I=KNPAST+1,IW(LPART+2))
C
  200 CALL QWPART (0,'HU')
 1011 FORMAT (' MC particle table. Particles 1 ...',I6,
     &  ' are standard particles'/
     &  '   MC# name',10X,'MC# ...')
 1012 FORMAT ((1X,7(I5,1X,3A4)))
      END
#endif
