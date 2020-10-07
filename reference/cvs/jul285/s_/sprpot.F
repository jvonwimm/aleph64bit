      SUBROUTINE SPRPOT(IER)
C----------------------------------------------------------------------
C! Create SATR pot banks
C!
C!    Author:     H. Meinhard       14-Mar-1989
C!    Modified:   H. Meinhard       02-Mar-1990  (4)
C!
C!    Input:      - banks SCOO, STCP
C!    Output:     - banks PSCO, PSPO
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "pscojj.h"
#include "psnamc.h"
#include "pspojj.h"
#include "rlunit.h"
#include "sanamc.h"
#include "scoojj.h"
#include "stcpjj.h"
C A32MAX is the maximum number that can be represented by a 32 bit int
      PARAMETER (A32MAX=.2147483E10)
#include "bmacro.h"
C----------------------------------------------------------------------
C link to existing banks
      JSCOO = IW(NASCOO)
      JSTCP = IW(NASTCP)
C
C number of objects
      NCOO = 0
      NTRK = 0
      IF (JSCOO .NE. 0) NCOO = LROWS(JSCOO)
      IF (JSTCP .NE. 0) NTRK = LROWS(JSTCP)
C
C create and fill PSCO bank
      IF (NCOO .NE. 0) THEN
C
        CALL AUBOS('PSCO',0,LMHLEN+NCOO*LPSCOA,JPSCO,IGARB)
        IF (JPSCO .EQ. 0 .OR. IGARB .EQ. 2)                 GOTO 902
        IF (IGARB .EQ. 1) THEN
          JSCOO = IW(NASCOO)
          JSTCP = IW(NASTCP)
          JPSCO = IW(NAPSCO)
        ENDIF
C
        IW(JPSCO+LMHCOL) = LPSCOA
        IW(JPSCO+LMHROW) = NCOO
C
        DO 300 ICOO = 1, NCOO
          KPSCO = KROW(JPSCO,ICOO)
          IW(KPSCO+JPSCSI) = ITABL(JSCOO,ICOO,JSCOSI) - 1
          IW(KPSCO+JPSCLA) = ITABL(JSCOO,ICOO,JSCOLA)
          IW(KPSCO+JPSCSC) = ITABL(JSCOO,ICOO,JSCOSC) - 1
          IW(KPSCO+JPSCTT) = INT(RTABL(JSCOO,ICOO,JSCOTT)*100000.)
          IW(KPSCO+JPSCTT+1) = INT(RTABL(JSCOO,ICOO,JSCOTT+1)*100000.)
  300   CONTINUE
C
      ENDIF
C
C create and fill PSPO bank
      IF (NTRK .NE. 0) THEN
C
        CALL AUBOS('PSPO',0,LMHLEN+NTRK*LPSPOA,JPSPO,IGARB)
        IF (JPSPO .EQ. 0 .OR. IGARB .EQ. 2)                 GOTO 903
        IF (IGARB .EQ. 1) THEN
          JSCOO = IW(NASCOO)
          JSTCP = IW(NASTCP)
          JPSPO = IW(NAPSPO)
        ENDIF
C
        IW(JPSPO+LMHCOL) = LPSPOA
        IW(JPSPO+LMHROW) = NTRK
C
        DO 320 ITRK = 1, NTRK
          KPSPO = KROW(JPSPO,ITRK)
          DO 310 ICOR = 1, LPSPOA
            IW(KPSPO+JPSPTC+ICOR-1) = ITABL(JSTCP,ITRK,JSTCTC+ICOR-1)
  310     CONTINUE
  320   CONTINUE
C
      ENDIF
C
      GOTO 999
C----------------------------------------------------------------------
  902 IER = 2
      CALL RERROR('SPRPOT',-IER,'No space for new bank PSCO')
      GOTO 999
  903 IER = 3
      CALL RERROR('SPRPOT',-IER,'No space for new bank PSPO')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
