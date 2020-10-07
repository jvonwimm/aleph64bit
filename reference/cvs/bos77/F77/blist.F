      SUBROUTINE BLIST(JW,OPT,LIST)
C
C     OPERATION WITH LISTS
C
C     OPT = 'LO'  L IS 'C','E','R','S','T' AND O IS'=','+','-'
C
C
C
      INTEGER JW(*)
      CHARACTER*4 NAME
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      CHARACTER*2 OPT
      CHARACTER*(*) LIST
C     ...
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'BLIST')
      JW(1)=0
      JW2  =0
      IL=INDEX('CERST',OPT(1:1))
      IP=INDEX('=+-',OPT(2:2))
      IF(IL.EQ.0.OR.IP.EQ.0) THEN
C
C        WRONG ARGUMENTS
C
         IF(JW(LUP).GT.0) WRITE(JW(LUP),1001) OPT,LIST
         CALL BABEND('BLIST')
      END IF
      IL=IL+ILT
      IF(JW(IL).EQ.0) THEN
         CALL WBANC(JW,JW(IL),0,*91)
         JW(JW(IL)-3)=INTCHA('+LST')
      END IF
      ID=JW(IL)
      NL=JW(ID)
      IF(IP.EQ.1) THEN
         IP=2
         NL=0
      END IF
      ND=JW(ID-2)-4
#include "listnn.h"
      IF(NAMI.NE.0) THEN
         IF(IP.EQ.2) THEN
C
C           ADD TO LIST
C
            DO 10 I=1,NL
   10       IF(JW(I+ID).EQ.NAMI) GOTO 3
            IF(NL.EQ.ND) THEN
               JW(ID)=NL
               ND=ND+10
               CALL WBANC(JW,JW(IL),ND,*91)
               ID=JW(IL)
            END IF
            NL=NL+1
            JW(ID+NL)=NAMI
            GOTO 3
C
C        DELETE FROM LIST
C
         ELSE
         NLL=NL
         DO 30 I=1,NLL
            IF(JW(I+ID).EQ.NAMI) THEN
               NL=NL-1
               DO 20 J=I,NL
   20          JW(J+ID)=JW(J+ID+1)
               GOTO 3
            END IF
   30       CONTINUE
            GOTO 3
         END IF
      END IF
      JW(ID)=NL
      JW(1)=0
      JW(2)=0
      GOTO 100
C
C     INSUFFICIENT SPACE
C
   91 CALL BBSPC(JW,'BLIST')
C
  100 RETURN
 1001 FORMAT('0BOS--BLIST-  WRONG ARGUMENT - STOP'/
     1       14X,'OPT,LIST= ',A4,A)
      END
