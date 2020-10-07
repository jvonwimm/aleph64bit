      SUBROUTINE BRWND(LUN)
C
C     REWIND UNIT LUN
C
#include "boscom.h"
#include "param.h"
#include "mparam.h"
*     ...
      IBF= NLINK('+BUF',LUN)
      IF(IBF.NE.0) THEN
#include "unpackio.h"
         IF(IOMD.EQ.0) THEN
            CALL BOSRW(LUN)
         ELSE IF(IOMD.EQ.1) THEN
            CALL BEPRW(LUN)
         END IF
      END IF
  100 RETURN
      END
