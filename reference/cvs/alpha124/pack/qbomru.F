      SUBROUTINE QBOMRU
CKEY BOM /INTERNAL
C----------------------------------------------------------------------
C! Get run-by-run BOM constants from database.
C                                                   R. Forty   02.02.92
C  Called from QMNEWR
C----------------------------------------------------------------------
#ifndef DOC
C
#include "bomajj.h"
#include "qcde.h"
      INTEGER ALGTRO
      COMMON /QBMDAT/ QBOMXO, QBOMXS, QBOMYO, QBOMYS, KBOMFL
#include "bmacro.h"
      IRET = ALGTRO ('BOMA', KRUN, JBOMRU, IROW)
C
      IF (IRET .LE. 0) THEN
        KBOMFL = -4
      ELSE
        KBOMA  = IRET
        QBOMXO = RTABL (KBOMA, IABS(IROW), JBOMXO)
        QBOMXS = RTABL (KBOMA, IABS(IROW), JBOMXS)
        QBOMYO = RTABL (KBOMA, IABS(IROW), JBOMYO)
        QBOMYS = RTABL (KBOMA, IABS(IROW), JBOMYS)
        KBOMFL = ITABL (KBOMA, IABS(IROW), JBOMFL)
      ENDIF
C
      END
#endif
