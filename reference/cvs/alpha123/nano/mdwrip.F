      SUBROUTINE MDWRIP(IRET)
CKEY NANO in ALPHA / INTERNAL
C----------------------------------------------------------------------
C!   Creates the DBTG bank for the MINI-DST from the output of QIPBTAG
C!
C!   Author   :- Dave Brown               5-DEC-1993
C!   Modified :- Agnieszka Jacholkowska   6-OCT-1994
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - IRET   / I   Return flag: 1 = success
C!                                    0 = else
C!
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
#include "ncomip.h"
#include "dbtgjj.h"
C
C!..Local variables
C
      INTEGER IND,IRET,BNKLEN,IROW,IGARB
      INTEGER ITRK
#include "qmacro.h"
C-----------------------------------------------------------------------
C
      IRET = 1
      IF(NBTTRK.EQ.0) THEN
        IRET=0
        RETURN
      ENDIF
C
C!..Make bank
C
      BNKLEN = LDBTGA*NBTTRK+LMHLEN
      CALL AUBOS('DBTG',1,BNKLEN,IND,IGARB)
      IF(IGARB.EQ.2)THEN
        CALL QWMESE('### MDWRIP ### No space to book DBTG')
        IRET=0
        RETURN
      ENDIF
      CALL BKFMT('DBTG','2I,(2I)')
C
C!..Pack mini-header
C
      IW(IND+1) = LDBTGA
      IW(IND+2) = NBTTRK
      IROW = IND + LMHLEN
      DO ITRK=1,NBTTRK
        IW(IROW+JDBTTF) = INIPAR(ITRK,1)
        IW(IROW+JDBTTS) = INIPAR(ITRK,2)
        IROW = IROW+LDBTGA
      END DO
C
C Add DBTG bank to MINI list
      CALL MINLIS ('DBTG')
C
C!..Done
C
  999 RETURN
      END
#endif
