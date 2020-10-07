      SUBROUTINE DCORR(ID,YA,YB,XA,XB)
      COMMON/HCS/KW(100)
      REAL XW(1)
      EQUIVALENCE (XW(1),KW(1))
#include "bosmdl.h"
#include "concom.h"

      IF(IBS.LT.0.OR.ID.LT.0) GOTO 100
      NA=ID
      IF(IBS.EQ.0) THEN
         IF(NA.GT.999999999) GOTO 100
      ELSE
         IF(NA.GT.99) GOTO 100
         NA=NA+IBS
      END IF

      IND=MLINK(KW,'CORR',NA)
      IF(IND.EQ.0) THEN
         IND=MBANK(KW,'CORR',NA,700)
         IF(IND.EQ.0) GOTO 100
         IF(IBS.NE.0) THEN
            KW(IND+1)=IHE(7)
            KW(IND+2)=IHE(8)
         END IF
         CALL BKFMT('CORR','20A,I,4F,I,3F,I,2F,I,7F,I,9F,(I)')
      ELSE
         IF(KW(IND+21).NE.0) GOTO 100
         IF(XW(IND+23).NE.0.0) GOTO 100
      END IF
      XW(IND+22)=AMIN1(YA,YB)
      XW(IND+23)=ABS(YB-YA)/50.0
      XW(IND+24)=AMIN1(XA,XB)
      XW(IND+25)=ABS(XB-XA)/100.0
  100 RETURN
      END
