      SUBROUTINE BEPBF(LUN,NBLK16)
C
C     DEFINE BUFFER FOR EPIO
C
#include "boscom.h"
#include "param.h"
#include "mparam.h"
C     ...
      IBF= NLINK('+BUF',LUN)
      IF(IBF.EQ.0) GOTO 10
#include "unpackio.h"
      IF(IOMD.NE.1) THEN
*        not EPIO mode
         IF(IW(LUP).GT.0) WRITE(IW(LUP),1001) LUN
         CALL BABEND('BEPBF')
      END IF
      GOTO 100
C
C     CREATE BUFFER BANK
C
   10 IBF= NBANK('+BUF',LUN,20)
      IF(IBF.EQ.0) GOTO 20
      IW(IBF+1)=10
      IOAC=0
      IOMD=1
      IOST=0
#include "packio.h"
*     NUMBER OF 16-BIT WORDS FOR EPIO RECORDS
*     (=MULTIPLE OF 180)
      IW(IBF+2)=180*(1+(NBLK16-1)/180)
      IF(NBLK16.EQ.0) IW(IBF+2)=0
      GOTO 100
   20 CALL BBSPC(IW,'BEPBF')
  100 RETURN
 1001 FORMAT('0BOS--BEPBF-  WRONG UNIT - STOP',
     1       14X,'UNIT =',I3)
      END
