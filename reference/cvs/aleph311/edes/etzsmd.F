      SUBROUTINE ETZSMD(X,SC,MD,NATZON,ZONE)
C-----------------------------------------------------------------------
CKEY ECALDES MODULE SENSITIVE ZONE / USER
C     H.Videau      Creation 17/08/86   Modification 01/12/86
C! Module sensitive zone
C  The sensitive part of a module is a convex volume delimited by a set
C  of planes. Hence the linear forms corresponding to the planes can be
C  defined such that there values on an inner point be positive. To
C  classify the outer regions in the vicinity of the inside we consider
C  that a point can be on the wrong side of:
C     1 plane    face region
C     2 planes   edge region
C     3 planes   corner region
C  The logical variable NATZON tells you if you are inside: .TRUE.
C  or outside the module: .FALSE.
C  We consider three ways to be outside: in Z, in R or in PHI.
C  The three words of ZONE (INTEGER) tell you then where you are:
C  Zone(1)= -1  the Z of your point is too small
C            0  your point is inside
C           +1  the Z of your point is too large
C  Zone(2)= -1  the R of your point is too small
C            0  your point is inside
C           +1  the R of your point is too large
C  Zone(3)= -1  the PHI of your point is too small
C            0  your point is inside
C           +1  the PHI of your point is too large
C  Remark that for the endcaps Rout is defined by the OR of 3 planes
C and that in the barrel Phineg and Phipos are defined by the OR of 2.
C   Input :
C           X(3)              real
C           SC,MD             integers
C   Output:
C           NATZON            logical
C           ZONE(3)           integer
C  NOTE: a precise test of the interstack insensitive region is not done
C in this routine.
C The test in the case of a local system is still not implemented.
C   Calls: none
C   Called by USER ,EFNCRS ,EFNDTW.
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Locales
      INTEGER I
      REAL SIGNE
C    Input
      REAL X(ECALLS-1)
      INTEGER SC,MD
C    Output
      LOGICAL NATZON
      INTEGER ZONE(3)
C
C     check validity of input
        IF(SC.LE.0.OR.SC.GT.ECALSC) GO TO 999
        IF(MD.LE.0.OR.MD.GT.ECALMD) GO TO 999
C
      NATZON=.TRUE.
      ZONE(1) = 0
      ZONE(2) = 0
      ZONE(3) = 0
C
      DO 2 I=1,ECALNP
      SIGNE=X(1)*EALILP(1,I,MD,SC)+X(2)*EALILP(2,I,MD,SC)+
     &      X(3)*EALILP(3,I,MD,SC)+     EALILP(4,I,MD,SC)
      IF(SIGNE.LT.0.) THEN
          ZONE(EVLSZN(I,SC)) = EVLSVL(I,SC)
          NATZON=.FALSE.
                      END IF
 2    CONTINUE
C
  999 RETURN
      END
#endif
