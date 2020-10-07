      SUBROUTINE ENTREE(SYSTM,SC,MD,PLAN)
C----------------------------------------------------------------------
CKEY ECALDES MODULE ENTRANCE PLANE / USER
C     H.Videau      Creation 22/12/86   Modification 04/11/88
C! Gives module entrance plane
C  Give in PLAN the coefficients in the system defined by SYSTM ( ALEPH
C  or LOCAL) of the  plane corresponding to the entrance of the module
C  MD of the subcomponent SC of the electromagnetic calorimeter.
C  If MD = 0 the module is the module type for the subcomponent.
C   Input :
C           SC,MD  integers
C           SYSTM  ch5
C   Output:
C           PLAN dim 4
C   Calls: none
C   Called by USER.
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
C    input variables
      INTEGER SC,MD
      CHARACTER*5 SYSTM
C    output variable
      REAL PLAN(ECALLS)
C    locales
      INTEGER I
C
      IF(SYSTM.EQ.'ALEPH') THEN
        IF(MD.EQ.0) THEN
        DO 3 I=1,ECALLS
        PLAN(I)= -ESSCPC(2,1,SC) * EBPLLF(I,ESCOS1(SC)) +
     &            ESSCPC(1,1,SC) * EBPLLF(I,ESCOS2(SC))
 3      CONTINUE
                   ELSE
        DO 1 I=1,ECALLS
        PLAN(I)= -ESSCPC(2,1,SC) * EALIPL(I,1,MD,SC)+
     &            ESSCPC(1,1,SC) * EALIPL(I,2,MD,SC)
 1      CONTINUE
                    END IF
                            ELSE
        DO 2 I=1,ECALLS
        PLAN(I)=-ESSCPC(2,1,ELOCES) * ELOCPL(I,1)+
     &           ESSCPC(1,1,ELOCES) * ELOCPL(I,2)
 2      CONTINUE
                            END IF
      END
#endif
