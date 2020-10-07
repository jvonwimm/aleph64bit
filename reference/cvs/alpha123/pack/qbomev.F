      SUBROUTINE QBOMEV
CKEY BOM /INTERNAL
C----------------------------------------------------------------------
C! Calculate beam position from BOMs.
C                                                   R. Forty   02.02.92
C  Called from QFHEAD
C----------------------------------------------------------------------
#ifndef DOC
C
#include "qcde.h"
#include "bombjj.h"
      COMMON /QBMDAT/ QBOMXO, QBOMXS, QBOMYO, QBOMYS, KBOMFL
#include "bmacro.h"
C
      KBOMB = IW (NABOMB)
C
      IF (KBOMB .EQ. 0) THEN
        KERBOM = -5
      ELSE
        XX = RTABL (KBOMB, 1, JBOMXX)
        YY = RTABL (KBOMB, 1, JBOMYY)
        IE = ITABL (KBOMB, 1, JBOMEE)
C
        IF (IE .LT. 0) THEN
          KERBOM = IE
        ELSE
          KERBOM = KBOMFL
        ENDIF
      ENDIF
C
      IF (KERBOM .GE. 0) THEN
        QVXBOM = XX * QBOMXS + QBOMXO
        QVYBOM = YY * QBOMYS + QBOMYO
      ELSE
        QVXBOM = QVXNOM
        QVYBOM = QVYNOM
      ENDIF
C
      END
#endif
