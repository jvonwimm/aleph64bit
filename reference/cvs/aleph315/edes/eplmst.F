      SUBROUTINE EPLMST(SYSTM,SC,MD,ST,PLAN)
C-----------------------------------------------------------------------
CKEY ECALDES STACK MIDDLE PLANE / USER
C     H.Videau      creation 25/10/86   modification 04/11/88
C                                                   M.Rumpf   03/19/87
C! Gives stack middle plane
C  This routine returns in PLAN the equation of the mid plane of the
C  stack ST in the module MD of the subcomponent SC.
C  Le vecteur normal au plan est norme.
C  mid plane veut dire plan bissecteur des deux plans limites de stacks
C  This plane contains the barycentres of the storeys of the stack.
C  If MD = 0 the module is the module type for the subcomponent.
C   Input :
C          SYSTM    coordinate system name (example 'ALEPH')   ch5
C          SC       sub component #     integer
C          MD       module  #              "
C          ST       Stack #                "
C   Output:
C          PLAN      4 coefficients defining mid plane of the stack ST
C   Calls: EPLNOR
C   Called by USER.
C------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C   Variables d'input
        CHARACTER*5 SYSTM
        INTEGER SC,MD,ST
C   Variables d'ouput
        REAL PLAN(ECALLS)
C     Locales
        INTEGER I
C
C
      IF(SYSTM.EQ.'ALEPH') THEN
         IF(MD.EQ.0) THEN
        DO 3 I=1,ECALLS
        PLAN(I)= -(ESSCPC(2,ST,SC)+ESSCPS(2,ST,SC)) *
     &                   * EBPLLF(I,ESCOS1(SC))+
     &            (ESSCPC(1,ST,SC)+ESSCPS(1,ST,SC)) *
     &                   * EBPLLF(I,ESCOS2(SC))
 3      CONTINUE
                     ELSE
        DO 1 I=1,ECALLS
        PLAN(I)= -(ESSCPC(2,ST,SC)+ESSCPS(2,ST,SC)) *
     &                    EALIPL(I,1,MD,SC)+
     &            (ESSCPC(1,ST,SC)+ESSCPS(1,ST,SC)) *
     &                    EALIPL(I,2,MD,SC)
 1      CONTINUE
                      END IF
                           ELSE
        DO 2 I=1,ECALLS
        PLAN(I)=-(ESSCPC(2,ST,ELOCES)+ESSCPS(2,ST,ELOCES)) *ELOCPL(I,1)+
     &           (ESSCPC(1,ST,ELOCES)+ESSCPS(1,ST,ELOCES)) *ELOCPL(I,2)
 2      CONTINUE
                           END IF
C    Normaliser les plans
        CALL EPLNOR(PLAN)
        END
#endif
