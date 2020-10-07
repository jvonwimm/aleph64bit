      SUBROUTINE EALGMK(SC,MD,NMARKS,MARKS)
C.----------------------------------------------------------------------
CKEY ECALDES ALIGNMENT MARKS  / INTERNAL
C     H.Videau      creation le 27/02/88           modifie le 04/11/88
C! Gets the alignment marks
C   Input :
C           SC   subcomponent number
C           MD   module number  (0 for module type)
C   Output:
C           NMARKS number of marks (mires) per module     INTEGER
C           MARKS(3,NMARKS) the coordinates of the marks  REAL
C   Calls: EROTPT
C   Called by USER (alignement).
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
C  input variables
      INTEGER MD,SC
C  output variables
      INTEGER NMARKS
      REAL MARKS(ECALLS-1,*)
C   Locales
      INTEGER I,J
C
C     We pick the mire positions in EQTY
      NMARKS=EQTYMN(ESCOEQ(SC))
      DO 1 I=1,NMARKS
       DO 2 J=1,ECALLS-1
        MARKS(J,I)=EQTYTP(J,I,ESCOEQ(SC))
 2     CONTINUE
C Place them in the module place  if MD.NE.0
       IF (MD.NE.0)
     &  CALL EROTPT(MARKS(1,I),MARKS(1,I),EALITD(1,1,MD,SC))
 1    CONTINUE
      END
#endif
