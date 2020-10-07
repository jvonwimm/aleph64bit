      FUNCTION MLINK(JW,NAME,NR)
#include "boscom.h"
#include "param.h"
      INTEGER JW(*)
#include "namcha.h"
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'MLINK')
   10 CONTINUE
#include "namez.h"
   20 JW(1)=0
      JW(2)=0
      INDI=0
      IF(NAMI.EQ.0) GOTO 90
      JND=JW(IDL)+NAMI-NSYST
      IF(JW(JND).NE.0) GOTO 40
      GOTO 30
C
C     SWITCH OFF BINARY SEARCH
C
   25 CALL WDROP(JW,JW(JND))
   30 CONTINUE
C
C     USE SEQUENTIAL SEARCH
C
#include "index.h"
      IF(NLPL.GT.NLPLM) CALL WBANK(JW,JW(JND),0,*90)
      GOTO 90
C
C     USE BINARY SEARCH
C
   40 IF(JW(JW(JND)).NE.0) GOTO 70
C
C     COUNT NUMBER OF BANKS
C
      NB=0
      IND=NAMI+1
   50 IND=JW(IND-1)
      IF(IND.NE.0) THEN
         NB=NB+1
         GOTO 50
      END IF
      IF(NB.LT.NLPLM) GOTO 25
      IF(JW(JW(JND)-2)-4.LT.2*NB) THEN
         CALL WBANK(JW,JW(JND),2*NB+10,*25)
      END IF
      JW(JW(JND))=2*NB
C
C     FILL NUMBER/INDEX PAIRS INTO BANK
C
      KND=JW(JND)
      IND=NAMI+1
   60 IND=JW(IND-1)
      IF(IND.NE.0) THEN
         JW(KND+1)=JW(IND-2)
         JW(KND+2)=IND
         KND=KND+2
         GOTO 60
      END IF
C
C     FIND NUMBER BY BINARY SEARCH IN LINK WORK BANK
C
   70 JND=JW(JND)
      INDI=0
      IL=JND+1
      IH=JND+JW(JND)-1
      I=IH
      IF(JW(IL).LE.NR.AND.JW(IH).GE.NR) THEN
      IF(JW(IL).EQ.NR) THEN
         INDI=JW(IL+1)
         GOTO 90
      END IF
   80    IF(JW(I).LT.NR) THEN
            IL=I
      ELSE
            IH=I
         END IF
         I=IL+((IH-IL)/4)*2
      IF(I.NE.IL) GOTO 80
      IF(JW(I).EQ.NR) INDI=JW(I+1)
      IF(JW(IH).EQ.NR) INDI=JW(IH+1)
      END IF
   90 IND=INDI
      JW(1)=0
      JW(2)=0
      GOTO 100
C
C     ENTRY WITH NAME-INDEX INSTEAD OF CHAR NAME
C
      ENTRY MLINC(JW,NAMIN,NR)
      NAMI=NAMIN
      GOTO 20
C
C     ENTRY WITH INTEGER NAME INSTEAD OF CHAR NAME
C
      ENTRY MLINX(JW,NAMAA,NR)
      NAMA=NAMAA
      GOTO 10
C
  100 MLINK=IND
      RETURN
      END
