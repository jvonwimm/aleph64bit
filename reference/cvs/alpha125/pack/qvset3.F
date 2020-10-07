      SUBROUTINE QVSET3 (ITK,P)
CKEY VECTOR COPY /USER
C----------------------------------------------------------------------
C! array P --> 3-vector(ITK)
C                                                    H.Albrecht  2.1.89
C                                       Modified:    E. Blucher  8.3.90
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      DIMENSION P(3)
      DATA IERR /0/
      J1(J) = KOQVEC + ITK * KCQVEC + J
C----------------------------------------------------------------------
C
      RW(J1(JQVEQX)) = P(1)
      RW(J1(JQVEQY)) = P(2)
      RW(J1(JQVEQZ)) = P(3)
      RW(J1(JQVEQP)) = SQRT (P(1)**2 + P(2)**2 + P(3)**2)
      RW(J1(JQVEQE)) = SQRT (RW(J1(JQVEQM))**2 + RW(J1(JQVEQP))**2)
C
      IF (ITK .LE. KLMCT .AND. ITK .GE. KFRET)  THEN
        IERR = IERR + 1
        IF (IERR .LE. 10)  CALL QWMESE
     &    ('_QVSET3_ Warning : Unviolable particle is modified')
      ENDIF
C
      END
#endif
