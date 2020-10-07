      FUNCTION NPRNT(NAME,NR)
C
C     PRINT BANK (NAME,NR) AND RETURN INDEX
C
      CHARACTER*4 NAME
      INTEGER JW(1000)
      EQUIVALENCE (IW(1),JW(1))
#include "boscom.h"
#include "param.h"
C     ...
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'NPRNT')
      JW(1)=0
      JW(2)=0
      IND=0
      IF(JW(LUP).EQ.0.OR.JW(LUB).EQ.0) GOTO 100
      IND =NLINK(  NAME,NR)
      IF(IND.EQ.0) GOTO 100
      CALL WPRNT(IW,IND)
  100 NPRNT=IND
      RETURN
      END
