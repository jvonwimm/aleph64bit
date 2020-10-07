      SUBROUTINE BWRSB(JW,LUN,NAME,NR)

*     write single bank on unit LUN

      INTEGER JW(*)
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      REAL RW(1000)
      EQUIVALENCE (IW(1),RW(1))
      CHARACTER*4 NAME
C     ...
      JW1=JW(1)
      JW(1)=0
C     IBF= NPRNT('+BUF',LUN)
      IBF= NLINK('+BUF',LUN)
      JW(1)=JW1
      IF(IBF.EQ.0) THEN
         CALL BOSWS(JW,LUN,NAME,NR)
      ELSE
#include "unpackio.h"
         IF(IOST.EQ.1) CALL BABEND('WRONG UNIT IN BWRSB')
         IF(IOMD.EQ. 0) THEN
           CALL BOSWS(JW,LUN,NAME,NR)
         ELSE IF(IOMD.EQ.1) THEN
           CALL BEPWS(JW,LUN,NAME,NR)
         ELSE IF(IOMD.EQ.2) THEN
           CALL BTEWS(JW,LUN,NAME,NR)
         ELSE
           CALL BABEND('WRONG UNIT IN BWRSB')
         END IF
      END IF
  100 RETURN
      END
