      SUBROUTINE BOSBK(JW)
C
C     PRINT TABLE OF ALL BANKS
C
      INTEGER JW(*)
      CHARACTER*4 CHAINT
#include "boscom.h"
#include "param.h"
      CHARACTER*25 TEXTN,TEXTW
      CHARACTER*11 TEXT
      INTEGER LINE(3,5)
      DATA    TEXTN/'    NAME.......NR....ND  '/
      DATA    TEXTW/'    NAME.......NT....ND  '/
C     ...
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'BOSBK')
      LUNP=JW(LUP)
      IF(LUNP.EQ.0) GOTO 100
      WRITE(LUNP,1001)   JW(NJA),CHAINT(JW(IAR+1)),CHAINT(JW(IAR+2))
C
C     NAMED BANKS
C
      TEXT='NAMED BANKS'
      WRITE(LUNP,1002) TEXT
      WRITE(LUNP,1003) (TEXTN,L=1,5)
      NDEL=0
      I   =0
      IND=JW(INM)+3
   10 IF(IND.GT.JW(IGP)) GOTO 20
      IF(JW(IND).LT.0) THEN
C
C        DELETED BANK
C
         NDEL=NDEL-JW(IND)
         IND =IND -JW(IND)
      ELSE
C
C        NOT DELETED BANK
C
         IF(I.GE.5) THEN
            WRITE(LUNP,1004) (CHAINT(LINE(1,J)),(LINE(K,J),K=2,3),J=1,I)
            I=0
         END IF
         I=I+1
         LINE(1,I)=JW(IND-3)
         LINE(2,I)=JW(IND-2)
         LINE(3,I)=JW(IND  )
         IND=IND+JW(IND)+4
      END IF
      GOTO 10
   20 IF(I.NE.0) WRITE(LUNP,1004) (CHAINT(LINE(1,J)),(LINE(K,J),
     +           K=2,3),J=1,I)
      WRITE(LUNP,1005) NDEL
C
C     WORK BANKS
C
      TEXT='WORK BANKS '
      WRITE(LUNP,1002) TEXT
      WRITE(LUNP,1003) (TEXTW,L=1,5)
      NDEL=0
      I   =0
      IND=JW(IWK)+3
   30 IF(IND.GT.JW(NJW)) GOTO 40
      IF(JW(IND).LT.0) THEN
C
C        DELETED BANK
C
         NDEL=NDEL-JW(IND)
      ELSE
C
C        NOT DELETED BANK
C
         IF(I.GE.5) THEN
            WRITE(LUNP,1004) (CHAINT(LINE(1,J)),(LINE(K,J),K=2,3),J=1,I)
            I=0
         END IF
         I=I+1
         LINE(1,I)=JW(IND-3)
         LINE(2,I)=JW(IND-2)
         LINE(3,I)=JW(IND  )
      END IF
      IND=IND+JW(IND-2)
      GOTO 30
   40 IF(I.NE.0) WRITE(LUNP,1004) (CHAINT(LINE(1,J)),(LINE(K,J),
     +           K=2,3),J=1,I)
      WRITE(LUNP,1005) NDEL
C
  100 RETURN
 1001 FORMAT('0BOS++BOSBK+  ARRAY NR',I3,5X,2A4,5X,'TABLE OF ALL BANKS')
 1002 FORMAT('0-----------  ',A)
 1003 FORMAT('0',5(A))
 1004 FORMAT(1X,5(4X,A4,I9,I6,2X))
 1005 FORMAT('0    PLUS',I10,' DELETED WORDS')
      END
