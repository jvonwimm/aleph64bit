      SUBROUTINE EFIXI94
C
C!    DROP RUN HEADER EKLS EKPL BANK
C     M.N Minard                    9-10-94
C     M.N Minard              mod  24-03-95
#ifndef DOC
#include "rcurnt.h"
#include "bcs.h"
#include "bmacro.h"
      IF (IRUNRC.GT.25000.AND.IRUNRC.LT.29090) THEN
        CALL BDROP(IW,'EKLSEKPL')
        IF (IRUNRC.LT.25937) THEN
C
C Artificially built EKLS bank of length Zero
C
          CALL AUBOS('EKLS',IRUNRC,LMHLEN,KEKLS,IGARB)
          IF ( IGARB.LT.2) THEN
            IW(KEKLS+LMHROW) = 0
            IW(KEKLS+LMHCOL) = 1
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
#endif
