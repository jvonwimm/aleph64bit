      SUBROUTINE VBSCLR
C----------------------------------------------------------------------
C!  - End of Run Beam Spot position computation, close bank
C!
C!   Author   :- E. Lancon             29-APR-1993
C!
C!   Bux fix: Steve Wasserbaech, April 1995
C!   Do not create a new row if the first row is not yet filled
C!   Modified: SW, 18 October 1995
C!   Switch to VBPC; introduce VBSP card.
C?
C!======================================================================
#ifndef DOC
C     IMPLICIT NONE
      SAVE FIRST, NAVBPC
#include "bcs.h"
#include "vbpcjj.h"
#include "vbscom.h"
C
      INTEGER NAVBPC, KVBPC, IP, JP, KBPOS, LREC, LDUM, IER, I
      INTEGER NAMIND, MALREC
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C
#include "bmacrod.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NAVBPC = NAMIND('VBPC')
      ENDIF
      KVBPC = IW(NAVBPC)
      IF (KVBPC .LE. 0) GO TO 999
C
C  Make sure the VBPC row number (= LEP energy regime) is valid:
C
      IF (IVBPC .LE. 0) GO TO 999
C
C   Now we handle the normal julia mode (no VBSP):
C
      IF (NCHOP .EQ. 0) THEN
C
C  If there are enough tracks (or if this is the only block
C  in the run), fit using only the current block.  But don't
C  create a new row if the first row is not yet filled in.
C
        IP = IBSTRK(IBSBUF)
        JP = IBSTRK(JBSBUF)
        IF ((LROWS(IP) .GE. ITABL(KVBPC,IVBPC,JVBPNU)) .AND.
     >      (IBSFEV .NE. 1)) THEN
          IW(JP + LMHROW) = 0
          CALL VBMFIT(.TRUE.)
        ELSE
          CALL VBMFIT(.FALSE.)
        ENDIF
C
C  Now we handle the case of a VBSP card:
C
      ELSE
        DO I=1,NCHOP
          ICHOP = I
          CALL VBMFIT((I .NE. 1))
        ENDDO
      ENDIF
C
C  Compress ALPB bank:
C
      CALL AUBPRS('ALPB')
C
C  Write the file:
C
      KBPOS = IW(NAMIND('BPOS'))
      IF (KBPOS .GT. 0) THEN
        CALL BLIST(IW, 'T=', '0')
        LREC = 72
        LDUM = MALREC('CARD',LREC)
        CALL AGTFIL('BPOS', 'WRIT', IBSUNI, IER)
        IF (IER .NE. 0) GO TO 998
        CALL BLIST(IW,'T+','ALPB')
        CALL BWRITE(IW,IBSUNI,'T')
        CALL BWRITE(IW,IBSUNI,'0')
  998   CONTINUE
        CALL ACLOSE(IBSUNI,IER)
      ENDIF
C
  999 RETURN
      END
#endif
