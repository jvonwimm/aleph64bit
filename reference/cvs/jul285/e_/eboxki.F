      SUBROUTINE EBOXKI
C----------------------------------------------------------------------
C!  - Kill ECAL box
C!
C!                 Author M.N Minard
C!                        5/06/90
C!     Bank created EDKB
C!     Bank used EBOK
C!     Called from EGETDS
C!     Calls to EKNEWB , EKNEWE
C!     Reference ALGTDB
C?
C!======================================================================
#ifndef DOC
#include "rlunit.h"
#include "rcurnt.h"
#include "ebokjj.h"
#include "edkbjj.h"
#include "bcs.h"
      EXTERNAL NAMIND
      INTEGER ALGTDB
#include "bmacro.h"
C
C? Drop edkb bank, if any
C
      CALL NDROP('EDKB',0)
C
C
C-    Read ebok if any
C
      NAEBOK = NAMIND ('EBOK')
      IF ( IW(NAEBOK).EQ.0) THEN
C
C-      Read from database
C
        IND = ALGTDB(LRCONS,'EBOK',IRUNRC)
        IF ( IND.EQ.0) GO TO 900
        NAEBOK = NAMIND('EBOK')
      ENDIF
C
C-    Now built EKDB BANK or expand it
C
      KEBOK = IW(NAEBOK)
      NEBOK = 0
      IF (KEBOK.NE.0) NEBOK = LROWS(KEBOK)
C
C-    Now decode index
C
      DO 10 IEBOK = 1,NEBOK
      ISC = ITABL(KEBOK,IEBOK,1)
      IF ( ISC.EQ.2) THEN
           CALL EKNEWB(ITABL(KEBOK,IEBOK,2))
      ELSE
           CALL EKNEWE(ITABL(KEBOK,IEBOK,2),ISC)
      ENDIF
C
C-    Update pointers
C
      KEBOK = IW(NAEBOK)
 10   CONTINUE
 900  CONTINUE
      RETURN
      END
#endif
