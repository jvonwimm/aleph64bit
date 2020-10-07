      SUBROUTINE UDBOOK
*     UPDATE BOOK,0 BANK
      COMMON/BCS/IW(1000)
#include "concom.h"
      DATA IDATE/0/
      IF(IDATE.EQ.0) CALL DATIME(IDATE,IDUMMY)
      CALL TMODUL(' ')
      IAD=0
      IRD=0
      DO 10 I=1,NHN
      IF(IHB(5,I).EQ.0) GOTO 10
      IF(IHB(5,I).GT.0) THEN
         IRD=IRD+1
      ELSE
         IAD=IAD+1
      END IF
   10 CONTINUE
      IF(IAD+IRD.EQ.0) GOTO 100
      IND=NLINK('BOOK',0)
      IF(IND.EQ.0) THEN
         ND=0
         CALL BKFMT('BOOK','(2A,I)')
         CALL BLIST(IW,'E+','BOOK')
      ELSE
         ND=IW(IND)
      END IF
      IND=NBANK('BOOK',0,ND+3*IAD)
      IF(IND.EQ.0) GOTO 100
      K =ND
      ND=ND+3*IAD
      DO 20 I=1,NHN
      IF(IHB(5,I).EQ.0) GOTO 20
      J=IHB(5,I)
      IF(J.GT.0) THEN
         IF(J.GT.ND) GOTO 20
         IW(IND+J)=IDATE
      ELSE
         K=K+3
         IF(K.GT.ND) GOTO 20
         IW(IND+K-2)=IHB(7,I)
         IW(IND+K-1)=IHB(8,I)
         IW(IND+K  )=IDATE
      END IF
      IHB(5,I)=0
   20 CONTINUE
      GOTO 100

      ENTRY PRBOOK
      IF(IW(6).EQ.0) GOTO 100
      IND=NLINK('BOOK',0)
      IF(IND.EQ.0) GOTO 100
      WRITE(IW(6),101) (IW(IND+I),I=1,IW(IND))
  100 RETURN
  101 FORMAT(' BOOK ---   ',5(2A4,I8,5X))
      END
