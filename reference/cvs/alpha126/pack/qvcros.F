      SUBROUTINE QVCROS (ITKRES,ITK1,ITK2)
CKEY VECTOR KINEMATICS /USER
C----------------------------------------------------------------------
C! 4-vector(ITK1) cross 4-vector(ITK2) --> 4-vector(ITKRES)
C                                                    H.Albrecht  2.1.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      DIMENSION ITKN(2)
      DATA IERR /0/
C----------------------------------------------------------------------
      JR(J) = KOQVEC + ITKRES * KCQVEC + J
      J1(J) = KOQVEC + ITK1   * KCQVEC + J
      J2(J) = KOQVEC + ITK2   * KCQVEC + J
      JS(J) = KOQVEC + J * KCQVEC + JQVESP
C----------------------------------------------------------------------
C       basic track attributes :
C
      RW(JR(JQVEQX)) = RW(J1(JQVEQY)) * RW(J2(JQVEQZ)) -
     &                 RW(J1(JQVEQZ)) * RW(J2(JQVEQY))
      RW(JR(JQVEQY)) = RW(J1(JQVEQZ)) * RW(J2(JQVEQX)) -
     &                 RW(J1(JQVEQX)) * RW(J2(JQVEQZ))
      RW(JR(JQVEQZ)) = RW(J1(JQVEQX)) * RW(J2(JQVEQY)) -
     &                 RW(J1(JQVEQY)) * RW(J2(JQVEQX))
      RW(JR(JQVEQP)) = SQRT (RW(JR(JQVEQX))**2 + RW(JR(JQVEQY))**2 +
     &   RW(JR(JQVEQZ))**2)
      RW(JR(JQVEQE)) = RW(JR(JQVEQP))
      RW(JR(JQVEQM)) = 0.
      RW(JR(JQVECH)) = RW(J1(JQVECH)) + RW(J2(JQVECH))
C
      IF (ITKRES .LE. KLUST .OR. ITK1 .LE. KLUST .OR.
     &  ITK2 .LE. KLUST)  GO TO 90
C
C       particle code, class :
C
      IW(JR(JQVEPA)) = 0
      ICLASS = IW(J1(JQVECL))
      IF (ICLASS .EQ. 0)  ICLASS = IW(J2(JQVECL))
      IW(JR(JQVECL)) = ICLASS
C
      IF (ICLASS .NE. IW(J2(JQVECL)))  CALL QMTERM
     &    ('_QVCROS_ Particles from different classes mixed up')
C
C       KSAME pointer :
C
      ITKS0 = IW(JS(ITKRES))
      ITKS1 = ITKS0
   10 ITKS2 = IW(JS(ITKS1))
      IF (ITKS2 .NE. ITKRES)  THEN
        ITKS1 = ITKS2
        GO TO 10
      ENDIF
      IW(JS(ITKS1)) = ITKS0
      IW(JS(ITKRES)) = ITKRES
C
C       mother - daughter - mother relation, match :
C
      ITKN(1) = ITK1
      ITKN(2) = ITK2
      CALL QADAU (ITKRES, 2, ITKN)
      IW(JR(JQVENO)) = 0
      IW(JR(JQVENM)) = IW(J1(JQVENM))
      IW(JR(JQVEML)) = IW(J1(JQVEML))
C
C       bit masks :
C
      DO 9 IB=1,KLOCKM
      IW(JR(JQVEBM+IB-1))
     & = IOR (IW(J1(JQVEBM+IB-1)),IW(J2(JQVEBM+IB-1)))
    9  CONTINUE
      IW(JR(JQVELK)) = 0
C
C       vertices :
C
      IW(JR(JQVESC)) = -1
      IW(JR(JQVEOV)) = IW(J1(JQVEOV))
      IW(JR(JQVEEV)) = IW(J1(JQVEOV))
      RW(JR(JQVEDB)) = 0.
      RW(JR(JQVEZB)) = 0.
      RW(JR(JQVESD)) = 0.
      RW(JR(JQVESZ)) = 0.
      RW(JR(JQVECB)) = 0.
C
C       other pointers etc. :
C
      IW(JR(JQVETN)) = 0
      IW(JR(JQVEQD)) = KOQDET
      IW(JR(JQVEKS)) = 0
C
C       error matrix
C
      RW(JR(JQVEEM)) = -1.
      RW(JR(JQVECF)) = -1.
C
   90 IF (ITKRES .GT. KLUST .AND. ITKRES .LE. KLMCT)  THEN
        IERR = IERR + 1
        IF (IERR .LE. 10)  CALL QWMESE
     &    ('_QVCROS_ Warning : Unviolable particle has been modified')
      ENDIF
C
      END
#endif
