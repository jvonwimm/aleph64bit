      SUBROUTINE BDASQ(JW,LUN1,LUN2)
*
*     COPY BANKS FROM DA DATA SET TO SEQUENTIAL DATA SET
*
#include "boscom.h"
#include "param.h"
      INTEGER JW(*)
      CHARACTER*4 NAME,CHAINT
      N1=0
      N2=0
      IUB=NLINK('+BUF',LUN1)
      IF(IUB.EQ.0) GOTO 100
      IW(IUB+ 9)=0
      IW(IUB+10)=0
   10 IND=MDARS(JW,LUN1)
      IF(JW(2).NE.0) GOTO 20
      N1=N1+1
      IF(IND.EQ.0) THEN
         N2=N2+1
      ELSE
         NAME=CHAINT(JW(IND-3))
         NR  =       JW(IND-2)
         JW(1)=1
         CALL BWRSB(JW,LUN2,NAME,NR)
         CALL BGARB(JW)
      END IF
      GOTO 10
   20 IF(JW(LUP).GT.0) WRITE(JW(LUP),101) LUN1,LUN2,N1,N2
         CALL BRWND(LUN2)
  100 RETURN
  101 FORMAT(' ---- BDASQ   BANKS COPIED FROM DA-DATA SET',I3,
     +   ' TO SEQ DATA SET',I3/
     +   1X,I10,' BANKS READ',I3,' NOT COPIED (SPACE INSUFF.)')
      END
