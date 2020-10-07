      SUBROUTINE ECONE(IPA,ENE)
C------------------------------------------------------------------
C------------------------------------------------------------------
#include "lundcom.h"
      PARAMETER( QQPI= 3.141592 )
      PARAMETER( COSEC= .972 )
C
      ENE= 0.
      DO 10 L1=1,n7lu
       IF(L1.EQ.IPA) GOTO 10
       IF(KCUP(L1).EQ.2) GOTO 10
       ASCA= SCALAR(L1,IPA,1,3)
       IF(ASCA.LT.COSEC) GOTO 10
       IF(KCUP(L1).EQ.1) THEN
        ENE= ENE+p7lu(L1,4)
       ELSE
        ITP= ABS(k7lu(L1,2))
        IF(ITP.EQ.ITPELE) THEN
         CALL GAUSSE(L1,EPAR)
         ENE= ENE+EPAR
        ELSE IF(ITP.EQ.ITPMU) THEN
         CALL LANDEP(EDEP)
         ENE= ENE+EDEP
        ELSE
         ENEP= PLU(L1,8)
         IF(ENEP.GT.(0.)) CALL PIDEP(ENEP,EDEP)
         ENE= ENE+EDEP
        END IF
       END IF
10    CONTINUE
C
      RETURN
      END
