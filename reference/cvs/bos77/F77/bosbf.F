      SUBROUTINE BOSBF(LUN,NBLK)
C
C     OPEN SEQUENTIAL FILE
C
#include "boscom.h"
#include "param.h"
C     ...
      IBF= NLINK('+BUF',LUN)
      IF(IBF.NE.0) GOTO 100
C
C     CREATE BUFFER BANK
C
   10 IBF= NBANK('+BUF',LUN,11)
      IF(IBF.EQ.0) GOTO 100
      IW(IBF+1)=0
      IW(IBF+2)=NBLK
  100 RETURN
      END
