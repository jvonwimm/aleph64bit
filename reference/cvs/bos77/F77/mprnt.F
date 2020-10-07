      FUNCTION MPRNT(JW,NAME,NR)
C
C     PRINT BANK (NAME,NR) AND RETURN INDEX
C
      CHARACTER*4 NAME
      INTEGER JW(*)
#include "boscom.h"
#include "param.h"
C     ...
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'MPRNT')
      JW(1)=0
      JW(2)=0
      IND=0
      IF(JW(LUP).EQ.0.OR.JW(LUB).EQ.0) GOTO 100
      IND=MLINK(JW,NAME,NR)
      IF(IND.EQ.0) GOTO 100
      CALL WPRNT(JW,IND)
  100 MPRNT=IND
      RETURN
      END
