      SUBROUTINE MINTBP
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill vertex bank DTBP for Mini-DST.
C
C     Author: Stephen Haywood      04-Apr-90
C
C     Input  : XTRB(or XTEB) bank
C     Output : DTBP bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "xtrbjj.h"
#include "xtebjj.h"
#include "dtbpjj.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Pick up trigger banks.
C
      KXTRB = NLINK('XTRB',0)
      KXTEB = NLINK('XTEB',0)
      IF(KXTRB.LE.0 .AND. KXTEB.LE.0) RETURN
C
C++   Create the DTBP bank.
C
      LEN = LMHLEN + LDTBPA
      CALL AUBOS('DTBP',0,LEN, KDTBP,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINTBP: Cannot create DTBP bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KXTRB = NLINK('XTRB',0)
         KXTEB = NLINK('XTEB',0)
      ENDIF
      IW(KDTBP+LMHCOL) = LDTBPA
      IW(KDTBP+LMHROW) = 1
C
C++   Copy words from trigger banks.
C
      IF(KXTRB.GT.0) THEN
         ITRG1 = ITABL(KXTRB,1,JXTRRP+0)
         ITRG2 = ITABL(KXTRB,1,JXTRRP+1)
         ITRG3 = ITABL(KXTRB,1,JXTRRP+2)
      ELSE
         ITRG1 = ITABL(KXTEB,1,JXTET1)
         ITRG2 = ITABL(KXTEB,1,JXTET2)
         ITRG3 = ITABL(KXTEB,1,JXTEL2)
      ENDIF
C
      IW(KROW(KDTBP,1)+JDTBT1) = ITRG1
      IW(KROW(KDTBP,1)+JDTBT2) = ITRG2
      IW(KROW(KDTBP,1)+JDTBL2) = ITRG3
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DTBP')
C
      RETURN
      END
#endif
