      SUBROUTINE TCORR(ID,TEXT)
      COMMON/HCS/KW(100)
      REAL XW(1)
      EQUIVALENCE (XW(1),KW(1))
#include "bosmdl.h"
#include "concom.h"
      CHARACTER*(*) TEXT
      CHARACTER*72 COMT
      IF(IBS.LT.0.OR.ID.LE.0) GOTO 100
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
         CALL BKFMT('CORR','20A,I,4F,I,3F,I,2F,I,7F,I,9F,(I)')
         IF(IBS.NE.0) THEN
            KW(IND+1)=IHE(7)
            KW(IND+2)=IHE(8)
         END IF
      END IF
      COMT=TEXT
      DO 70 I=1,18
   70 KW(IND+2+I)=INTCHA(COMT(4*I-3:4*I))
  100 RETURN
      END
