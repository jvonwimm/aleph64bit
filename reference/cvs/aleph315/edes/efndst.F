      INTEGER FUNCTION EFNDST(SC,MD,X,SYSTM)
C.----------------------------------------------------------------------
CKEY ECALDES STACK / USER
C     H.Videau      creation 15/12/85   modification  6/06/88
C! Finds Stack number
C  Looks in the module MD of the subcomponent SC for the
C  stack containing the point X.
C  If the point is outside a stack (in particular in the intermediate
C  plates) ST is set to 0.
C   Input :
C           SC      sub component #                  INTEGER
C           MD      module #          integer        INTEGER
C           X       point coordinates in             REAL(3)
C           SYSTM   coordinate system name           CHARACTER*5
C                   ('ALEPH' for example)
C   Output:
C           EFNDST  Stack #    1<= EFNDST<= 3        INTEGER
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C   Input variables
      REAL X(ECALLS-1)
      CHARACTER*5 SYSTM
      INTEGER MD,SC
C    Locales
      INTEGER ST
      REAL LOC1,LOC2,TEST
C
C
      IF(SYSTM.EQ.'ALEPH') THEN
       LOC1= EALIPL(1,1,MD,SC)*X(1)+EALIPL(2,1,MD,SC)*X(2)+
     &       EALIPL(3,1,MD,SC)*X(3)+EALIPL(4,1,MD,SC)
       LOC2= EALIPL(1,2,MD,SC)*X(1)+EALIPL(2,2,MD,SC)*X(2)+
     &       EALIPL(3,2,MD,SC)*X(3)+EALIPL(4,2,MD,SC)
                           ELSE
       LOC1= ELOCPL(1,1)*X(1)+ELOCPL(2,1)*X(2)+
     &       ELOCPL(3,1)*X(3)+ELOCPL(4,1)
       LOC2= ELOCPL(1,2)*X(1)+ELOCPL(2,2)*X(2)+
     &       ELOCPL(3,2)*X(3)+ELOCPL(4,2)
                           END IF
      DO 2 ST=1 ,ECALST
         TEST=-LOC2*ESSCPC(1,ST,SC)+LOC1*ESSCPC(2,ST,SC)
           IF(TEST.GE.0.) GO TO 100
 2    CONTINUE
      ST=ECALST+1
 100    CONTINUE
      ST=ST-1
      IF (ST.GT.0) THEN
C    teste que l'on n'est pas au dela du stack
       TEST= -ESSCPS(1,ST,SC) * LOC2 + ESSCPS(2,ST,SC) * LOC1
      IF(TEST.LT.0.) ST=0
                   END IF
      EFNDST = ST
      END
#endif
