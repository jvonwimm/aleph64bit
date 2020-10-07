      SUBROUTINE ABBNIN(JW,LUN,NXT,NTO)

*     CHECK BANK LENGTH AND INSERT NAMEINDEX INTO POS. JW(IND-1)

      INTEGER JW(*)
#include "boscom.h"
#include "param.h"
      CHARACTER*4 NAME,CHAINT
      JW2=1
C      IRD=MLINK(JW,'+REC',LUN)
C      IF(IRD.EQ.0) GOTO 90
C      NTOT=JW(IRD)
        NTOT    = NTO
C      IND=IRD+4
        IND = NXT+3
      NAMA=JW(IND-3)+1
C
C     NEXT BANK - CHECK LENGTH
C
   10 NW=JW(IND)+4
      IF(MIN(NTOT,NW).LT.4.OR.NW.GT.NTOT) THEN
C         IRD=MDROP(JW,'+REC',LUN)
         GOTO 90
      END IF
      IF(JW(IND-3).NE.NAMA) THEN
C
C        NEW NAME - GET NAMEINDEX
C
         NAMA=JW(IND-3)
#include "namez.h"
         IF(NAMI.EQ.0) THEN
            NAME=CHAINT(NAMA)
            NAMI=NAMIND(NAME)
         END IF
      END IF
      JW(IND-1)=NAMI
      IND=IND+NW
      NTOT=NTOT-NW
      IF(NTOT.NE.0) GOTO 10
      JW2=0
   90 JW(2)=JW2
  100 RETURN
      END
