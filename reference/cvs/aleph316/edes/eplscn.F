      INTEGER FUNCTION EPLSCN(PLAN,NPLANS,LISPL,POINT)
C.----------------------------------------------------------------------
CKEY ECALDES LINEAR SCAN  /INTERNAL
C     H.Videau      Creation 15/12/85   Modification le 27/10/87
C! Linear scan in planes pencil
C  Makes a linear scan through the planes of the pencil to determine
C  the ones which are around the point.
C   Calls: none
C   Called internally
C.----------------------------------------------------------------------
#ifndef DOC
C
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  Input variables
      INTEGER NPLANS
      REAL PLAN(ECALLS,ECALLP),LISPL(ECALLP,NPLANS),POINT(ECALLS-1)
C  Locales
      INTEGER IC
      REAL CL1,CL2,TEMP,TEMP2
C
C Calcul des coordonnees locales
      CL1=PLAN(1,1)*POINT(1)+PLAN(2,1)*POINT(2)+
     &    PLAN(3,1)*POINT(3)+PLAN(4,1)
      CL2=PLAN(1,2)*POINT(1)+PLAN(2,2)*POINT(2)+
     &    PLAN(3,2)*POINT(3)+PLAN(4,2)
C     Recherche lineaire des plans limitant
      DO 2033 IC=1 ,NPLANS-1
        TEMP=-CL2*LISPL(1,IC)+CL1*LISPL(2,IC)
        IF(TEMP.LT.0.) THEN
          TEMP2=-CL2*LISPL(1,IC+1)+CL1*LISPL(2,IC+1)
          IF(TEMP2.GE.0) THEN
            EPLSCN=IC
            GO TO 100
          END IF
        ELSE
          EPLSCN=0
          GO TO 100
        END IF
 2033 CONTINUE
      EPLSCN=NPLANS
 100  CONTINUE
      END
#endif
