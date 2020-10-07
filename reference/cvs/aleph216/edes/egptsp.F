      SUBROUTINE EGPTSP(PTSTI,PTSTS,PTPL)
C.----------------------------------------------------------------------
CKEY ECALDES PLANE LIMITS STACK  / INTERNAL
C     H.Videau      creation 16/09/87   modification 26/01/88
C! Gets points on stack and plane planes
C  This routine
C     searches for the stack volumes by name = VOLNAM
C     picks the plane coefficients related to this volume
C     chooses the front and end planes.
C     constructs points by projecting on them the origin. PTSTI,PTSTS
C     derives the pitches by dividing the stack thickness by the plane
C     number.   ESTYPP
C     constructs points on the planes by adding pitches to the stack
C     points.  PTPL
C   Input :
C           none
C   Output:
C           PTSTS(ECALLS,ECALST,ECALSC)                  4, 3,3
C           PTSTI(ECALLS,ECALST,ECALSC)                  4, 3,3
C           PTPL (ECALLS,ECALPL+1,ECALSC)                4,46,3
C   Calls  EVOLPL
C   Called by EDEFSP
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C   Output
      REAL PTSTI(ECALLS,ECALST,ECALSC)
      REAL PTSTS(ECALLS,ECALST,ECALSC)
      REAL PTPL (ECALLS,ECALPL+1,ECALSC)
C   Locales
      INTEGER I,PL,SC,ST
      INTEGER LEPLAN,ENTREE(ECALSC),SORTIE(ECALSC)
      REAL PLANES(ECALLS,10),PITSCA,LEPLS,VDOT
      CHARACTER*16 VOLNAM(ECALST,ECALSC)
      DATA VOLNAM/
     &'E stack1','E stack2','E stack3',
     &'B stack1','B stack2','B stack3',
     &'E stack1','E stack2','E stack3'/
      DATA ENTREE,SORTIE/6,1,6,5,2,5/
C
      DO 1 SC=1,ECALSC
        DO 2 ST=1,ECALST
          CALL EVOLPL(VOLNAM(ST,SC),SC,0,LEPLAN,PLANES)
          LEPLS= ESTKLS(ST)-ESTKFR(ST)+1
          DO 3 I=1,ECALLS-1
           PTSTI (I,ST,SC)=-PLANES(4,ENTREE(SC))*PLANES(I,ENTREE(SC))
           PTSTS (I,ST,SC)=-PLANES(4,SORTIE(SC))*PLANES(I,SORTIE(SC))
           ESSCPP(I,ST,SC)=(PTSTS(I,ST,SC)-PTSTI(I,ST,SC))/LEPLS
 3        CONTINUE
          PTSTI (4,ST,SC)=1.
          PTSTS (4,ST,SC)=1.
          ESSCPP(4,ST,SC)=0.
 2      CONTINUE
        DO 4 PL=1,ECALPL
          DO 5 I=1,ECALLS
            PTPL(I,PL,SC)=PTSTI(I,EPLNES(PL),SC)+
     &       FLOAT(PL-ESTKFR(EPLNES(PL)))*ESSCPP(I,EPLNES(PL),SC)
 5        CONTINUE
 4      CONTINUE
          DO 6 I=1,ECALLS
            PTPL(I,ECALPL+1,SC)=PTSTI(I,EPLNES(ECALPL),SC)+
     &FLOAT(ECALPL+1-ESTKFR(EPLNES(ECALPL)))*ESSCPP(I,EPLNES(ECALPL),SC)
 6        CONTINUE
 1    CONTINUE
        END
#endif
