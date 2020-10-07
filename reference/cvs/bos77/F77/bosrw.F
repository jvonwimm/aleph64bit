      SUBROUTINE BOSRW(LUN)
C
C     REWIND UNIT LUN (FORTRAN)
C
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      REAL RW(1000)
      EQUIVALENCE (IW(1),RW(1))
C     ...
      IBF= NLINK('+BUF',LUN)
      IF(IBF.EQ.0) GOTO 100
#include "unpackio.h"
      IF(IOMD.NE.0) THEN
         IF(IW(LUP).GT.0) WRITE(IW(LUP),1001) LUN
         CALL BABEND('BOSRW')
      END IF
      IF(IOST.EQ. 2.AND.IW(IBF+11).NE.0) THEN

*        WRITE LAST BUFFER BEFORE REWIND

         CALL BBFWRS(LUN,IOAC,IW(IBF+4)+1,IW(IBF+11),IW(IBF+2))
         IW(IBF+4)=IW(IBF+4)+1
         RW(IBF+8)=RW(IBF+8)+1.0E-6*FLOAT(NBYTWD*(IW(IBF+11)+1))
         IW(IBF+11)=0
         IF(IW(LUP).NE.0.AND.IW(LUW).GT.0) THEN
         IW(LUW)=IW(LUW)-1
         END IF
      END IF
      IOST=MOD(IOST,4)+4
      REWIND LUN
      IF(IW(LUP).GT.0) WRITE(IW(LUP),1002) LUN
#include "packio.h"
      IBF= NBANK('+BUF',LUN,11)
  100 RETURN
 1001 FORMAT('0BOS--BOSRW-  WRONG UNIT - STOP',
     1       14X,'UNIT =',I3)
 1002 FORMAT('0BOS--BOSRW-  REWIND UNIT',I3)
      END
