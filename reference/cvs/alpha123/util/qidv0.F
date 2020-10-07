      SUBROUTINE QIDV0 (ITKV0, CNAM1, CNAM2)
CKEY PART /USER
C----------------------------------------------------------------------
C! Calculate mass for particle pointing to V0
C                                                    H.Albrecht 6.11.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER *(*) CNAM1, CNAM2
      DIMENSION ITK(2)
C-----------------------------------------------------------------------
      JR(J) = KOQVEC + ITKV0  * KCQVEC + J
      J1(J) = KOQVEC + ITK(1) * KCQVEC + J
      J2(J) = KOQVEC + ITK(2) * KCQVEC + J
      PX(I) = RW(KOQVEC + ITK(I) * KCQVEC + JQVEQX)
      PY(I) = RW(KOQVEC + ITK(I) * KCQVEC + JQVEQY)
      PZ(I) = RW(KOQVEC + ITK(I) * KCQVEC + JQVEQZ)
      SXX(I) = RW(KOQVEC + ITK(I) * KCQVEC + JQVEEM  )
      SXY(I) = RW(KOQVEC + ITK(I) * KCQVEC + JQVEEM+1)
      SYY(I) = RW(KOQVEC + ITK(I) * KCQVEC + JQVEEM+2)
      SXZ(I) = RW(KOQVEC + ITK(I) * KCQVEC + JQVEEM+3)
      SYZ(I) = RW(KOQVEC + ITK(I) * KCQVEC + JQVEEM+4)
      SZZ(I) = RW(KOQVEC + ITK(I) * KCQVEC + JQVEEM+5)
C-----------------------------------------------------------------------
C       daughter particles
C
      IF (IW(JR(JQVEND)) .NE. 2)  THEN
        IERR = IERR + 1
        IF (IERR .LE. 10)  CALL QWMESE
     &    ('_QIDV0_ Particle does not decay into 2 particles')
        GO TO 90
      ENDIF
      ITK(1) = IW(KOQLIN+1+IW(JR(JQVEDL)))
      ITK(2) = IW(KOQLIN+2+IW(JR(JQVEDL)))
C
C       basic track attributes :
C
      RW(JR(JQVEQX)) = PX(1) + PX(2)
      RW(JR(JQVEQY)) = PY(1) + PY(2)
      RW(JR(JQVEQZ)) = PZ(1) + PZ(2)
      RW(JR(JQVEQP)) = SQRT (RW(JR(JQVEQX))**2 + RW(JR(JQVEQY))**2 +
     &   RW(JR(JQVEQZ))**2)
      E1 = SQRT (RW(J1(JQVEQP))**2 +
     &  RW(KOQPAR+KPART(CNAM1)*KCQPAR+JQPAMA)**2)
      E2 = SQRT (RW(J2(JQVEQP))**2 +
     &  RW(KOQPAR+KPART(CNAM2)*KCQPAR+JQPAMA)**2)
      RW(JR(JQVEQE)) = AMAX1 (E1 + E2, RW(JR(JQVEQP)))
      RW(JR(JQVEQM)) = SQRT ((RW(JR(JQVEQE)) + RW(JR(JQVEQP))) *
     &  (RW(JR(JQVEQE)) - RW(JR(JQVEQP))))
      RW(JR(JQVECH)) = RW(J1(JQVECH)) + RW(J2(JQVECH))
C
C       error matrix :
C
      IF (RW(J1(JQVEEM)) .GT. 0. .AND. RW(J2(JQVEEM)) .GT. 0.)  THEN
        RW(JR(JQVEEM  )) = SXX(1) + SXX(2)
        RW(JR(JQVEEM+1)) = SXY(1) + SXY(2)
        RW(JR(JQVEEM+2)) = SYY(1) + SYY(2)
        RW(JR(JQVEEM+3)) = SXZ(1) + SXZ(2)
        RW(JR(JQVEEM+4)) = SYZ(1) + SYZ(2)
        RW(JR(JQVEEM+5)) = SZZ(1) + SZZ(2)
        RW(JR(JQVEEM+6)) =
     &    (PX(1) * SXX(1) + PY(1) * SXY(1) + PZ(1) * SXZ(1)) / E1 +
     &    (PX(2) * SXX(2) + PY(2) * SXY(2) + PZ(2) * SXZ(2)) / E2
        RW(JR(JQVEEM+7)) =
     &    (PX(1) * SXY(1) + PY(1) * SYY(1) + PZ(1) * SYZ(1)) / E1 +
     &    (PX(2) * SXY(2) + PY(2) * SYY(2) + PZ(2) * SYZ(2)) / E2
        RW(JR(JQVEEM+8)) =
     &    (PX(1) * SXZ(1) + PY(1) * SYZ(1) + PZ(1) * SZZ(1)) / E1 +
     &    (PX(2) * SXZ(2) + PY(2) * SYZ(2) + PZ(2) * SZZ(2)) / E2
        RW(JR(JQVEEM+9)) =
     &   (PX(1)**2 * SXX(1) + PY(1)**2 * SYY(1) + PZ(1)**2 * SZZ(1) +
     &   2. * (PX(1) * (PY(1) * SXY(1) + PZ(1) * SXZ(1)) +
     &   PY(1) * PZ(1) * SYZ(1))) / E1**2 +
     &   (PX(2)**2 * SXX(2) + PY(2)**2 * SYY(2) + PZ(2)**2 * SZZ(2) +
     &   2. * (PX(2) * (PY(2) * SXY(2) + PZ(2) * SXZ(2)) +
     &   PY(2) * PZ(2) * SYZ(2))) / E2**2
      ELSE
        RW(JR(JQVEEM)) = -1.
      ENDIF
      RW(JR(JQVECF)) = -1.
C
 90   END
#endif
