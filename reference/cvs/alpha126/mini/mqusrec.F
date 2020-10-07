      SUBROUTINE MQUSREC
CKEY RUN /INTERNAL
C ----------------------------------------------------------------------
C MINI-DST official production : Store KSEC and RUNE banks
C A. Jacholkowska 7-Jan-1999
C Put in ALPHA 11-Jan-1999
C Called from QMSREC
C ----------------------------------------------------------------------
#ifndef DOC
#include "minprd.h"
#include "bcs.h"
#include "bmacro.h"
C ----------------------------------------------------------------------
C Do-nothing if not official MINI production :
      IF (IMIPRD.NE.1) GO TO 999
C++   Add KSEC and RUNE banks to Mini list.
C
      KKSEC = iw(namind('KSEC'))
      KRUNE = iw(namind('RUNE'))
C
      IF(KKSEC.GT.0) CALL MINLIS('KSEC')
      IF(KRUNE.GT.0) CALL MINLIS('RUNE')
      IF(KKSEC.GT.0.AND.KRUNE.GT.0) THEN
        ICOUNT = ICOUNT + 1
        CALL MINOUT
      ENDIF
C
 999  RETURN
      END
#endif
