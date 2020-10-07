      SUBROUTINE QVSET4 (ITK,P)
CKEY VECTOR COPY /USER
C----------------------------------------------------------------------
C! array P --> 4-vector(ITK)
C                                                    H.Albrecht  2.1.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      DIMENSION P(4)
      DATA IERR /0/
      J1(J) = KOQVEC + ITK * KCQVEC + J
C----------------------------------------------------------------------
C
      RW(J1(JQVEQX)) = P(1)
      RW(J1(JQVEQY)) = P(2)
      RW(J1(JQVEQZ)) = P(3)
      RW(J1(JQVEQE)) = P(4)
      RW(J1(JQVEQP)) = SQRT (P(1)**2 + P(2)**2 + P(3)**2)
      IF (RW(J1(JQVEQP)) .LE. P(4))  THEN
        RW(J1(JQVEQM)) = SQRT ((P(4) + RW(J1(JQVEQP))) *
     &                         (P(4) - RW(J1(JQVEQP))))
      ELSE
        IERR = IERR + 1
        IF (IERR .LE. 10)  CALL QWMESE
     &    ('_QVSET4_ Negative mass **2')
        RW(J1(JQVEQM)) = - SQRT ((RW(J1(JQVEQP)) + P(4)) *
     &                           (RW(J1(JQVEQP)) - P(4)))
      ENDIF
C
      IF (ITK .LE. KLMCT .AND. ITK .GE. KFRET)  THEN
        IERR = IERR + 1
        IF (IERR .LE. 10)  CALL QWMESE
     &    ('_QVSET4_ Warning : Unviolable particle is modified')
      ENDIF
C
      END
#endif
