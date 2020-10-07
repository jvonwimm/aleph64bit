      SUBROUTINE ETETRD(X,V1,V2,ROSE)
C.----------------------------------------------------------------------
CKEY ECALDES ORTHONORMED TETRAD / USER
C     H.Videau      Creation 15/12/85   Modification 23/12/85
C! Gives tetrad from X,V1,V2
C  Constructs in ROSE an orthonormed triad around V1 as first vector
C  with the (V1,V2)plane normal to the third axis.
C  Adds the displacement X to make a projective tetrad.
C   Calls: ENORME,CROSS
C   Called by USER ,ECSGEO in GALEPH
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Input
      REAL X(ECALLS-1),V1(ECALLS-1),V2(ECALLS-1)
C   Output
      REAL ROSE(ECALLS,ECALLS)
C    Locales
      INTEGER I
C
        DO 4 I=1,ECALLS-1
              ROSE(I,1)=V1(I)
              ROSE(I,ECALLS)=X(I)
 4       CONTINUE
        CALL ENORME(ROSE(1,1))
C       ROSE(I,3)=V1 ; V2
        CALL CROSS(V1,V2,ROSE(1,3))
        CALL ENORME ( ROSE(1,3))
C       ROSE(I,2)=ROSE(I,3);ROSE(I,1)
        CALL CROSS(ROSE(1,3),ROSE(1,1),ROSE(1,2))
        CALL ENORME(ROSE(1,2))
      ROSE(ECALLS,1)=0.
      ROSE(ECALLS,2)=0.
      ROSE(ECALLS,3)=0.
      ROSE(ECALLS,ECALLS)=1.
        END
#endif
