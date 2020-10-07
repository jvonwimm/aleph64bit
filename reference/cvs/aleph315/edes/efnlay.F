      INTEGER FUNCTION EFNLAY(SC,MD,X,SYSTM,INSTK)
C.----------------------------------------------------------------------
CKEY ECALDES PLANE SENSITIVE LIMITS / USER
C     H.Videau      Creation 10/10/88
C! Finds plane number (in sensitive zone)
C  Looks in the module MD of the subcomponent SC for the plane
C  containing the point X.
C  If the point is outside the sensitive zone of the module it returns 0
C   Input :
C           SC      sub component #                 INTEGER
C           MD      module #                        INTEGER
C           X       point coordinates in            REAL(3)
C           SYSTM  coordinate system name           CHARACTER*5
C                  ('ALEPH' for example)
C   Output:
C           EFNLAY  Plane #  1<= EFNLAY<= 45        INTEGER
C                           if in sensitive zone of the module
C                           if not EFNLAY = 0
C           INSTK  specifies if the point is in the stacks (.TRUE.)
C                              or in the interstack layers (.FALSE.)
C   Calls: EPLSQL .
C   Called by USER    (EHTRKE,EHSHOW in the MC)
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C     Input variables
      REAL X(ECALLS-1)
      CHARACTER*5 SYSTM
      INTEGER MD,SC
C   Locales
      INTEGER PL,ST,I
      REAL SIGNE
C    Called routines
      INTEGER EPLSQL
C
C   Output
       LOGICAL INSTK
C     Recherche du plan
      INSTK=.TRUE.
      IF(SYSTM.EQ.'ALEPH')   THEN
       PL=EPLSQL(EALIPL(1,1,MD,SC),ECALPL+1,EPSCPC(1,1,SC),X)
                             ELSE
       PL=EPLSQL(ELOCPL(1,1),ECALPL+1,EPSCPC(1,1,SC),X)
                             END IF
      IF(PL.GE.ECALPL+1.OR.PL.LE.0)  THEN
                         PL=0
                        INSTK=.FALSE.
                         ELSE
        ST = EPLNES(PL)
      IF(SYSTM.EQ.'ALEPH')   THEN
C Check of the intermediate unsensitive layers
      SIGNE=
     &(EALIPL(1,1,MD,SC)*X(1)+EALIPL(2,1,MD,SC)*X(2)+
     & EALIPL(3,1,MD,SC)*X(3)+EALIPL(4,1,MD,SC))*ESSCPS(2,ST,SC)-
     &(EALIPL(1,2,MD,SC)*X(1)+EALIPL(2,2,MD,SC)*X(2)+
     & EALIPL(3,2,MD,SC)*X(3)+EALIPL(4,2,MD,SC))*ESSCPS(1,ST,SC)
      IF(SIGNE.LT.0.) THEN
                   INSTK = .FALSE.
                      END IF
C Check the limits of the sensitive module
      DO 2 I=1,ECALNP
      SIGNE=X(1)*EALILP(1,I,MD,SC)+X(2)*EALILP(2,I,MD,SC)+
     &      X(3)*EALILP(3,I,MD,SC)+     EALILP(4,I,MD,SC)
      IF(SIGNE.LT.0.) THEN
                      PL = 0
                     INSTK=.FALSE.
                      GO TO 12
                      END IF
 2    CONTINUE
 12   CONTINUE
                             ELSE
C Check of the intermediate unsensitive layers
      SIGNE=
     &(ELOCPL(1,1)*X(1)+ELOCPL(2,1)*X(2)+
     & ELOCPL(3,1)*X(3)+ELOCPL(4,1))*ESSCPS(2,ST,SC)-
     &(ELOCPL(1,2)*X(1)+ELOCPL(2,2)*X(2)+
     & ELOCPL(3,2)*X(3)+ELOCPL(4,2))*ESSCPS(1,ST,SC)
      IF(SIGNE.LT.0.) THEN
                   INSTK = .FALSE.
                      END IF
C Check the limits of the sensitive module
      DO 3 I=1,ECALNP
      SIGNE=X(1)*ELOCLP(1,I)+X(2)*ELOCLP(2,I)+
     &      X(3)*ELOCLP(3,I)+     ELOCLP(4,I)
      IF(SIGNE.LT.0.) THEN
                     PL = 0
                    INSTK=.FALSE.
                     GO TO 13
                      END IF
 3    CONTINUE
 13   CONTINUE
                             END IF
                             END IF
      EFNLAY=PL
      END
#endif
