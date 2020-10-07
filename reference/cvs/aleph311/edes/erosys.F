      SUBROUTINE EROSYS(X,V,ROSE)
C.----------------------------------------------------------------------
CKEY ECALDES LOCAL SYSTEM / INTERNAL
C     H.Videau      Creation 15/12/85   Modification 23/12/85
C! Build local ref. system
C  Constructs in ROSE a triad around V as first vector
C  Adds the displacement X to make a projective tetrad.
C   Input :
C           X(3) : origin coordinates
C           V    : direction at X
C   Output:
C           Local tetrad ROSE
C   Called by ECSGEO in GALEPH
C   Calls: ENORME,CROSS
C.---------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Input
      REAL X(ECALLS-1),V(ECALLS-1)
C    Output
      REAL ROSE(ECALLS,ECALLS)
C    Locales
      REAL SR3
      INTEGER I
      LOGICAL INIT
      DATA INIT/.FALSE./
C
        IF(INIT) GO TO 100
        INIT=.TRUE.
        SR3=1./SQRT(3.)
 100       CONTINUE
C
        DO 4 I=1,ECALLS-1
              ROSE(I,1)=V(I)
              ROSE(I,ECALLS)=X(I)
 4       CONTINUE
        CALL ENORME(ROSE(1,1))
        DO 5 I=1,ECALLS-1
              ROSE(I,3)=0.
              IF(ABS(ROSE(I,1)).LT.SR3) ROSE(I,3)=1.
 5      CONTINUE
C       ROSE(I,2)=ROSE(I,3);ROSE(I,1)
        CALL CROSS(ROSE(1,3),ROSE(1,1),ROSE(1,2))
        CALL ENORME(ROSE(1,2))
C       ROSE(I,3)=ROSE(I,1);ROSE(I,2)
        CALL CROSS(ROSE(1,1),ROSE(1,2),ROSE(1,3))
      ROSE(ECALLS,1)=0.
      ROSE(ECALLS,2)=0.
      ROSE(ECALLS,3)=0.
      ROSE(ECALLS,ECALLS)=1.
        END
#endif
