      SUBROUTINE BEPRW(LUN)
C
C     REWIND UNIT LUN
C
#include "boscom.h"
#include "param.h"
#include "mparam.h"
C     ...
      IF(LEPIO.EQ.0) THEN
         CALL EPINIT
*        CALL EPSTAT
         LEPIO=1
*        IF(IW(6).NE.0) CALL EPIOT(IW(6))
      END IF
      IBF= NLINK('+BUF',LUN)
      IF(IBF.EQ.0) GOTO 100
#include "unpackio.h"
      IF(IOMD.NE.1) THEN
         IF(IW(LUP).GT.0) WRITE(IW(LUP),1001) LUN
         CALL BABEND('BEPRW')
      END IF
      IF(MOD(IOST,4).NE.0) THEN
         CALL EPRWND(LUN,IW(IBF+21),IER)
         IF(IOST.EQ.2) IW(IBF+4)=IW(IBF+4)+1
         IOST=MOD(IOST,4)+4
#include "packio.h"
         IF(IER.NE.0) GOTO 202
         IF(IW(LUP).NE.0.AND.IW(LUW).GT.0) THEN
            WRITE(IW(LUP),1003) LUN
            IW(LUW)=IW(LUW)-1
         END IF
      END IF
      IBF= NBANK('+BUF',LUN,20)
      IF(IBF.EQ.0) GOTO 201
  100 RETURN
  201 CALL BBSPC(IW,'BEPRW')
      GOTO 100
  202 IF(IW(LUP).GT.0) WRITE(IW(LUP),1002) IER
      CALL BABEND('BEPRW')
      GOTO 100
 1001 FORMAT('0BOS--BEPRW-  WRONG UNIT - STOP',
     1       14X,'UNIT =',I3)
 1002 FORMAT('0BOS--BEPRW-  EPIO ERROR',I3,' - STOP')
 1003 FORMAT('0BOS--BEPRW-  REWIND UNIT',I3)
      END
