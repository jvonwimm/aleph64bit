      SUBROUTINE QVSETC (ITK,ACHAR)
CKEY VECTOR KINEMATICS /USER
C----------------------------------------------------------------------
C! ACHAR   --> Charge(ITK)
C                                                    J.Boucrot 26.10.93
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      DATA IERR /0/
      J1(J) = KOQVEC + ITK * KCQVEC + J
C----------------------------------------------------------------------
C
      RW(J1(JQVECH)) = ACHAR
C
      IF (ITK .LE. KLMCT .AND. ITK .GE. KFRET)  THEN
        IERR = IERR + 1
        IF (IERR .LE. 10)  CALL QWMESE
     &    ('_QVSETC_ Warning : Unviolable particle is modified')
      ENDIF
C
      END
#endif
