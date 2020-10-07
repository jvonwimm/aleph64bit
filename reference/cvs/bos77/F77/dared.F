      SUBROUTINE DARED

*     REDUCE NUMBER OF ENTRIES IN WORK BANK

#include "dacom.h"
      COMMON/BCS/IW(1000)
      CALL SORTAI(IW(INW+1),1,NIW,1)
      IF(NIW.LE.100) GOTO 100

      DO 10 I=1,NIW
      IF(IW(INW+I).GT.NRDA) GOTO 20
   10 CONTINUE
      I=NIW
   20 IF(NEX.GT.0) THEN
         ILO=I-20
         IHI=I+80
      ELSE
         ILO=I-80
         IHI=I+20
      END IF
      IF(ILO.LT.1) THEN
         ILO=1
         IHI=ILO+99
      END IF
      IF(IHI.GT.NIW) THEN
         IHI=NIW
         ILO=IHI-99
      END IF
      IF(ILO.NE.1) THEN
         NRLO=IW(INW+ILO-1)
         JRLO=1
      END IF
      IF(IHI.NE.NIW) THEN
         NRHI=IW(INW+IHI+1)
         JRHI=1
      END IF
      NIW=IHI-ILO+1
      IF(ILO.NE.1) THEN
         DO 30 I=1,NIW
   30    IW(INW+I)=IW(INW+I+ILO-1)
      END IF
  100 RETURN
      END
