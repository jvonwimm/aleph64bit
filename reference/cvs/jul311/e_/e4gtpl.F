      SUBROUTINE E4GTPL(A3,B3,C3,D3,ISUB,IMO,STA)
C----------------------------------------------------------------------
C!  - R2 ESTIMATOR : GIVE EQUATION OF PLANS OF MODULE IMO OF SUBCOMP. IS
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!      this routine will be modified soon
C?
C!======================================================================
#ifndef DOC
#include "rlunit.h"
#include "rflags.h"
#include "rparac.h"
#include "e4par0.h"
      DIMENSION DDX(3),XX(2),YY(2)
      DIMENSION COCO(3),RORO(3)
C (COCO,RORO) POINT CHOSEN IN BARREL PLAN TO GET EQUATION
      DATA (COCO(K),K=1,3),(RORO(I),I=1,3)/65.,117.,163.,4.,28.,4./
CXX
CXX   PLAN EQUATION A3 X + B3 Y +C3 Z + D3 = 0
CXX
C
      IF(JDBDRF(JULEC).GT.8)
     &WRITE(LDEBRL,*)'E4GTPL ISUB,IMO,STA',ISUB,IMO,STA
CXX   DETERMINATION PLAN PARAMETERS
      IF(ISUB.EQ.2)THEN
         C3=0
C        RECHERCHE DE A3 B3 D3
C        WE TAKE 2 POINTS ON THE PLANE
         DO 1 IJK=1,2
         COL=COCO(IJK)
         ROW=RORO(IJK)+FLOAT((IMO-1))*32.
         CALL ESRPT('ALEPH',COL ,ROW ,STA,DDX)
         XX(IJK)=DDX(1)
  1      YY(IJK)=DDX(2)
         CALL E4RES2(XX,YY,A3,B3,D3,IERROR)
      ELSE
         A3=0
         B3=0
         C3=1
         COL=COR412
         IF(ISUB.EQ.3)COL=COR422
         ROW=4
         CALL ESRPT('ALEPH',COL ,ROW ,STA,DDX)
         D3=-DDX(3)
         ENDIF
      RETURN
      END
#endif
