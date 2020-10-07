      SUBROUTINE BKFRW(JW1,NAME,NR,JW2,ID,*)
C
C     COPY WORK BANK TO NAMED BANK
C
      INTEGER JW1(*),JW2(*),ID(*)
      CHARACTER*4 NAME
#include "boscom.h"
#include "mparam.h"
#include "param.h"
C     ...
      JW1(1)=0
      JW2(1)=0
      IF(JW2(ID(1)).LT.0) GOTO 10
      ND2=JW2(ID(1))
      NAMI=NAMIND(NAME)
      IFM=0
      IDF=IW(IDFMT+NAMI-NSYST)
      IF(IDF.NE.0) IFM=IW(IDF+3)
      IF(IFM.EQ.3) THEN
         ND1=(ND2-1)/NBYTWD+1
      ELSE IF(IFM.EQ.2) THEN
         ND1=(ND2-1)/(NBYTWD/2)+1
      ELSE IF(IFM.EQ.1) THEN
         ND1=ND2
      ELSE
         ND1=ND2
      END IF
      JW1(1)=1
      IND= MBANK(JW2,NAME,NR,ND1)
      IF(IND.EQ.0) GOTO 20
      JW1(IND+ND1)=0
      IF(IFM.EQ.3) THEN
         CALL BUN8W(JW2(ID(1)+1),1,JW1(IND+1),1,ND2)
      ELSE IF(IFM.EQ.2) THEN
         CALL BUN16W(JW2(ID(1)+1),1,JW1(IND+1),1,ND2)
      ELSE IF(IFM.EQ.1) THEN
         CALL BUN32W(JW2(ID(1)+1),1,JW1(IND+1),1,ND2)
      ELSE
         CALL UCOPY(JW2(ID(1)+1),JW1(IND+1),ND2)
      END IF
      GOTO 100
C
C     ERROR IN WBANK
C
   10 JW2(2)=0
      JW1(2)=5
      GOTO 30
C
C     INSUFFICIENT SPACE
C
   20 JWTWO=JW1(2)
      JW2(2)=0
      JW1(2)=JWTWO
   30 JW1(IRC+JW1(2))=JW1(IRC+JW1(2))+1
      IF(JW1(LUP).NE.0.AND.JW1(LUW).NE.0)
     +   CALL BBRET(JW1,JW1(2),'BKFRW',NAME)
      GOTO 101
C
  100 RETURN
  101 RETURN 1
      END
