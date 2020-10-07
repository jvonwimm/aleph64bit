      SUBROUTINE BUNIT(LUN,MODE,NBLK)

*     define unit parameters

#include "boscom.h"
#include "param.h"
#include "mparam.h"
      REAL RW(1000)
      EQUIVALENCE (IW(1),RW(1))
      CHARACTER*(*) MODE
      IBF=NLINK('+BUF',LUN)
*     ignore, if unit has buffer bank already
      IF(IBF.NE.0) GOTO 100
*
      IOAC=0
      IOMD=0
      IOST=0

      IF(INDEX(MODE,'SEQ').NE.0) IOAC=0
      IF(INDEX(MODE,'DIR').NE.0) IOAC=1
      IF(INDEX(MODE,'SPE').NE.0) IOAC=2

      IF(INDEX(MODE,'EPIO').NE.0) THEN
         IOMD=1
         CALL BEPBF(LUN,NBLK)
      ELSE IF(INDEX(MODE,'TEXT').NE.0) THEN
         IOMD=2
         CALL BTEBF(LUN,NBLK)
      ELSE
         IBF=NBANK('+BUF',LUN,11)
         IF(IBF.EQ.0) CALL BABEND('BUNIT')
         IW(IBF+1)=0
         IW(IBF+2)=NBLK
      END IF
      IBF=NLINK('+BUF',LUN)
      IF(IBF.EQ.0) GOTO 100
#include "packio.h"
  100 RETURN
      END
