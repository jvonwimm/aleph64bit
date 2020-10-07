      SUBROUTINE EVOLPL(VOLNAM,SC,MD,LEPLAN,PLANES)
C.----------------------------------------------------------------------
CKEY ECALDES VOLUME PLANE LIMIT / USER
C     H.Videau      creation 28/04/87   modification le 26/01/88
C! Gets planes limiting volume.
C   Input :
C           VOLNAM : name of the volume.     Ch16
C           SC     : subcomponent number    integer
C           MD     : module number          integer (can be 0)
C   Output:
C           LEPLAN : number of planes limiting the volume.
C           PLANES : the 4 coefficients of the planes.Dimension (4,<=8)
C This routine searches for the volume name = VOLNAM
C     picks the plane coefficients related to this volume
C     applies the corresponding sign to make the volume positive
C     applies the rotation corresponding to the slot if MD.NE.0
C   Calls: none.
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  input variables
      CHARACTER*(*) VOLNAM
      INTEGER SC,MD
C  output variables
      INTEGER LEPLAN
      REAL PLANES(ECALLS,*)
C  local variables
      INTEGER I,J,IDEFTY,IDEVOL,IDELNF
C
      LEPLAN = 0
C   looks for the volume
      DO 1 I=1,NEVOL
        IF(VOLNAM.EQ.EVOLNM(I)) GO TO 3
 1    CONTINUE
      GO TO 999
 3    CONTINUE
      IF(EVOLEQ(I).NE.ESCOEQ(SC)) GO TO 999
       IDEVOL=EVOLID(I)
C  Looks at the related planes by using the relation
C       inverse EVOL -> EVL
      LEPLAN = EVOLLS(IDEVOL)-EVOLFR(IDEVOL)+1
      DO 10 I=EVOLFR(IDEVOL),EVOLLS(IDEVOL)
        IDELNF= EVLFEL(I)
        IDEFTY= EVLFEA(I)
        DO 11 J=1,ECALLS
C transfers and puts the sign
          PLANES(J,IDEFTY)=EVLFSG(I)*ELNFLF(J,IDELNF)
 11    CONTINUE
C Place them in the module place  if MD.NE.0
      IF (MD.NE.0)
     & CALL EROTPL(PLANES(1,IDEFTY),PLANES(1,IDEFTY),EALITD(1,1,MD,SC))
      IF(MD.EQ.0.AND.SC.EQ.3)
     & CALL EROTPL(PLANES(1,IDEFTY),PLANES(1,IDEFTY),EECBDM(1,1))
 10   CONTINUE
 999  CONTINUE
      END
#endif
