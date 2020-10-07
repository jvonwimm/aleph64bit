      SUBROUTINE BWIND(JW,NAME,NRMAX,ID)
      INTEGER JW(*),ID(1)
      CHARACTER*4 NAME
#include "boscom.h"
#include "param.h"
C     ...
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'BWIND')
      JW(1)=0
      JW(2)=0
      N=0
      NAMI=NAMIND(NAME)
      IND=NAMI+1
   10 IND=JW(IND-1)
      IF(IND.NE.0) THEN
         IF(JW(IND-2).LE.0)     GOTO 10
         IF(JW(IND-2).GT.NRMAX) GOTO 20
         N=JW(IND-2)
         GOTO 10
      END IF
   20 CALL WBANC(JW,ID,N,*90)
*     STORE NAME IN WORK BANK
      JW(ID(1)-3)=IW(IDNAM+NAMI-NSYST)
      IF(N.EQ.0) GOTO 100
      DO 25 I=1,N
   25 JW(ID(1)+I)=0
C
C     INSERT INDICES
C
      IND=NAMI+1
   30 IND=JW(IND-1)
      IF(IND.NE.0) THEN
         IF(JW(IND-2).LE.0)     GOTO  30
         IF(JW(IND-2).GT.NRMAX) GOTO 100
         JW(ID(1)+JW(IND-2))=IND
         GOTO 30
      END IF
      GOTO 100
C
C     INSUFFICIENT SPACE
C
   90 CALL BBSPC(JW,'BWIND')
  100 RETURN
      END
