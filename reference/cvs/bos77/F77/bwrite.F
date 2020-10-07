      SUBROUTINE BWRITE(JW,LUN,LIST)
C
C     WRITE ON UNIT LUN
C
      INTEGER JW(*)
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      REAL RW(1000)
      EQUIVALENCE (IW(1),RW(1))
      CHARACTER*(*) LIST
*     ...
      IBF= NLINK('+BUF',LUN)
      IF(IBF.EQ.0) THEN
         CALL BOSWR(JW,LUN,LIST)
      ELSE
#include "unpackio.h"
         IF(IOST.EQ. 1) CALL BABEND('WRONG UNIT IN BWRITE')
         IF(IOMD.EQ. 0) THEN
            CALL BOSWR(JW,LUN,LIST)
         ELSE IF(IOMD.EQ.1) THEN
            CALL BEPWR(JW,LUN,LIST)
         ELSE IF(IOMD.EQ.2) THEN
            CALL BTEWR(JW,LUN,LIST)
         ELSE
            CALL BABEND('WRONG UNIT IN BWRITE')
         END IF
      END IF
      IF(JW(2).EQ.0) GOTO 100
      NWPTR=JW(2)
      CALL BUNDSL(LUN,NWPTR)
  100 RETURN
      END
