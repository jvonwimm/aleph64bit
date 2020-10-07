      SUBROUTINE BTEBF(LUN,NBLK)
C
*     open text file
*
#include "boscom.h"
#include "param.h"
*     ...
      IBF= NLINK('+BUF',LUN)
      IF(IBF.NE.0) GOTO 100

*     create buffer bank

   10 IBF= NBANK('+BUF',LUN,11)
      IF(IBF.EQ.0) CALL BABEND('BTEBF')
#include "unpackio.h"
      IOMD=2
#include "packio.h"
      IW(IBF+2)=NBLK
  100 RETURN
      END
