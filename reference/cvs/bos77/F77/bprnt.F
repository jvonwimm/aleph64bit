      SUBROUTINE BPRNT(JW,LIST)
C
C
C
#include "boscom.h"
#include "param.h"
      INTEGER JW(*)
      CHARACTER*(*) LIST
      CHARACTER*4   NAME
C     ...
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'BPRNT')
      JW1=JW(1)
      JW(1)=0
      JW(2)=0
      IF(JW(LUP).EQ.0.OR.JW(LUB).EQ.0) GOTO 100
#include "listnn.h"
      IF(NAMI.LE.0) GOTO 100
      IND=NAMI+1
   10 IND=JW(IND-1)
      IF(IND.EQ.0) GOTO 3
      CALL WPRNT(JW,IND)
      IF(JW1.NE.0) GOTO 3
      GOTO 10
C
  100 RETURN
      END
