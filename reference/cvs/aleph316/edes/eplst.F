      SUBROUTINE EPLST(SYSTM,SC,MD,ST,PLAN)
C-----------------------------------------------------------------------
CKEY ECALDES PLANE STACK / USER
C     H.Videau      Creation 25/10/86   modification 04/11/88
C! Gets a weighted plane in a stack
C  This routine returns in PLAN the equation of a plane of the
C  stack INT(ST) in the module MD of the subcomponent SC.
C  The plane is the barycentre of the two stack limiting planes with
C  weights 1.-FRACT(ST),  FRACT(ST) .
C  If MD = 0 the module is the module type for the subcomponent.
C  Le vecteur normal au plan est norme.
C   Input :
C          SYSTM    coordinate system name (example 'ALEPH') ch5
C          SC       sub component #      integer
C          MD       module  #            integer
C          ST       Stack # ( real)
C   Output:
C          PLAN      4 coefficients defining plane of the stack ST
C   Calls: EPLNOR
C   Called by USER.
C----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C   Variables d'input
        CHARACTER*5 SYSTM
        INTEGER SC,MD
        REAL ST
C   Variables d'ouput
        REAL PLAN(ECALLS)
C     Locales
        INTEGER I,IST
        REAL WST
C
      IST= INT(ST)
      WST= AMOD(ST,1.)
C
      IF(SYSTM.EQ.'ALEPH') THEN
         IF(MD.EQ.0) THEN
        DO 3 I=1,ECALLS
        PLAN(I)= -((1.-WST)*ESSCPC(2,IST,SC)+WST*ESSCPS(2,IST,SC))
     &                   * EBPLLF(I,ESCOS1(SC))+
     &            ((1.-WST)*ESSCPC(1,IST,SC)+WST*ESSCPS(1,IST,SC))
     &                   * EBPLLF(I,ESCOS2(SC))
 3      CONTINUE
                     ELSE
        DO 1 I=1,ECALLS
        PLAN(I)= -((1.-WST)*ESSCPC(2,IST,SC)+WST*ESSCPS(2,IST,SC))
     &                   * EALIPL(I,1,MD,SC)+
     &            ((1.-WST)*ESSCPC(1,IST,SC)+WST*ESSCPS(1,IST,SC))
     &                   * EALIPL(I,2,MD,SC)
 1      CONTINUE
                      END IF
                           ELSE
        DO 2 I=1,ECALLS
       PLAN(I)=-((1.-WST)*ESSCPC(2,IST,ELOCES)+WST*ESSCPS(2,IST,ELOCES))
     &                   *ELOCPL(I,1)+
     &          ((1.-WST)*ESSCPC(1,IST,ELOCES)+WST*ESSCPS(1,IST,ELOCES))
     &                   *ELOCPL(I,2)
 2      CONTINUE
                           END IF
C    Normaliser le plan
        CALL EPLNOR(PLAN)
        END
#endif
