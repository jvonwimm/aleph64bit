      SUBROUTINE QMCFRF
CKEY INIT CARDS /INTERNAL
C----------------------------------------------------------------------
C! read and interpret FRxy Data Cards
C called from QMCARD
C                                                   J.Boucrot  23.02.98
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      LOGICAL XFR10,XFR12
      INTEGER ACARD1
C----------------------------------------------------------------------
C BY DEFAULT XFRF2 HAS BEEN SET TO .TRUE. IN QMINIT
C
      XFRF0=ACARD1 ('FRF0') .NE. 0
      XFR10=ACARD1 ('FR10') .NE. 0
      XFR12=ACARD1 ('FR12') .NE. 0
C
C FRF0 data card :
C
      IF (XFRF0) THEN
        IF (ACARD1('FRF2').NE.0.OR.XFR10.OR.XFR12) THEN
          CALL QMTERM
     &    ('_QMCARD_ Cannot give both FRF0 and other FRxy cards.')
        ENDIF
        XFRF2=.FALSE.
      ENDIF
C
C FR10 data card:
C
      IF (XFR10) THEN
        IF (ACARD1('FRF2').NE.0.OR.XFRF0.OR.XFR12) THEN
          CALL QMTERM
     &    ('_QMCARD_ Cannot give both FR10 and other FRxy cards.')
        ENDIF
        XFRF2=.FALSE.
      ENDIF          
C
C FR12 data card:
C
      IF (XFR12) THEN
        IF (ACARD1('FRF2').NE.0.OR.XFRF0.OR.XFR10) THEN
          CALL QMTERM
     &    ('_QMCARD_ Cannot give both FR12 and other FRxy cards.')
        ENDIF
        XFRF2=.FALSE.
      ENDIF
C
 999  RETURN
      END
#endif
