      SUBROUTINE MINXTR
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill XTRB bank from DTBP.
C
C     Author: Stephen Haywood      18-Apr-91
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "dtbpjj.h"
#include "xtrbjj.h"
#include "bmacro.h"
C
C++   Pick up links.
C
      KDTBP = NLINK('DTBP',100)
      IF (KDTBP.LE.0) THEN
         CALL MINUPD('DTBP')
         KDTBP = NLINK('DTBP',100)
         IF (KDTBP.LE.0) RETURN
      ENDIF
C
C++   Create XTRB bank.
C
      LEN = LMHLEN + LXTRBA
      CALL AUBOS('XTRB',0,LEN, KXTRB,IGARB)
      CALL BLIST(IW,'S+','XTRB')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTBP = NLINK('DTBP',100)
      ENDIF
      IW(KXTRB+LMHCOL) = LXTRBA
      IW(KXTRB+LMHROW) = 1
C
      IW(KROW(KXTRB,1)+JXTRRP+0) = ITABL(KDTBP,1,JDTBT1)
      IW(KROW(KXTRB,1)+JXTRRP+1) = ITABL(KDTBP,1,JDTBT2)
      IW(KROW(KXTRB,1)+JXTRRP+2) = ITABL(KDTBP,1,JDTBL2)
      IW(KROW(KXTRB,1)+JXTRRP+3) = INTCHA('TRB1')
C
      RETURN
      END
#endif
