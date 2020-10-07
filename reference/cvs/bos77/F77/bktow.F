      SUBROUTINE BKTOW(JW1,NAME,NR,JW2,ID,*)
C
C     COPY NAMED BANK TO WORK BANK
C
      INTEGER JW1(*),JW2(*),ID(*)
      CHARACTER*4 NAME
#include "boscom.h"
#include "mparam.h"
#include "param.h"
C     ...
      JW1(1)=0
      JW2(1)=0
      IND= MLINK(JW1,NAME,NR)
      IF(IND.EQ.0) GOTO 10
      ND1=JW1(IND)
      IFM=0
      IDF=IW(IDFMT+NAMI-NSYST)
      IF(IDF.NE.0) IFM=IW(IDF+3)
      IF(IFM.EQ.3) THEN
         ND2=ND1*NBYTWD
      ELSE IF(IFM.EQ.2) THEN
         ND2=ND1*NBYTWD/2
      ELSE IF(IFM.EQ.1) THEN
         ND2=ND1*NBYTWD/4
      ELSE
         ND2=ND1
      END IF
      CALL WBANK(JW2,ID,ND2,*20)
      IF(IFM.EQ.3) THEN
         CALL BLO8W(JW1(IND+1),1,JW2(ID(1)+1),1,ND2)
      ELSE IF(IFM.EQ.2) THEN
         CALL BLO16W(JW1(IND+1),1,JW2(ID(1)+1),1,ND2)
      ELSE IF(IFM.EQ.1) THEN
         CALL BLO32W(JW1(IND+1),1,JW2(ID(1)+1),1,ND2)
      ELSE
         CALL UCOPY(JW1(IND+1),JW2(ID(1)+1),ND2)
      END IF
      GOTO 100
C
C     NAMED BANK NOT FOUND
C
   10 JW2(2)=0
      JW1(2)=6
      GOTO 30
C
C     INSUFFICIENT SPACE
C
   20 JWTWO=JW2(2)
      JW2(2)=0
      JW1(2)=JWTWO
   30 JW1(IRC+JW1(2))=JW1(IRC+JW1(2))+1
      IF(JW1(LUP).NE.0.AND.JW1(LUW).NE.0)
     +   CALL BBRET(JW1,JW1(2),'BKTOW',NAME)
      GOTO 101
C
  100 RETURN
  101 RETURN 1
      END
