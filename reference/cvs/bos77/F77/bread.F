      SUBROUTINE BREAD(JW,LUN,LIST,*,*)

*     READ FROM UNIT LUN

      INTEGER JW(*)
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      REAL RW(1000)
      EQUIVALENCE (IW(1),RW(1))
      CHARACTER*(*) LIST
*     ...
      IF(JW(1).EQ.0) CALL BINSEL(LUN,JW(1))
      JW1=JW(1)
      IBF= NLINK('+BUF',LUN)
      JW(1)=0
      IF(IBF.EQ.0) THEN
         JW(1)=JW1
         CALL BOSRD(JW,LUN,LIST,*101,*102)
      ELSE
#include "unpackio.h"
*        test read
         IF(IOST.EQ. 2) CALL BABEND('WRONG UNIT IN BREAD')
         JW(1)=JW1
         IF(IOMD.EQ. 0) THEN
            CALL BOSRD(JW,LUN,LIST,*101,*102)
         ELSE IF(IOMD.EQ.1) THEN
            CALL BEPRD(JW,LUN,LIST,*101,*102)
         ELSE IF(IOMD.EQ.2) THEN
            CALL BTERD(JW,LUN,LIST,*101,*102)
         ELSE
            CALL BABEND('WRONG UNIT IN BREAD')
         END IF
      END IF
  100 RETURN
  101 RETURN 1
  102 RETURN 2
      END
