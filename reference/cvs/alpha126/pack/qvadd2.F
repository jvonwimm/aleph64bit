      SUBROUTINE QVADD2 (ITKSUM,ITK1,ITK2)
CKEY VECTOR KINEMATICS /USER
C----------------------------------------------------------------------
C! Sum (4-vector(ITKn) (n=1,2)) --> 4-vector(ITKSUM)
C                                                    H.Albrecht  2.1.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      DATA IERR /0/
C----------------------------------------------------------------------
      JR(J) = KOQVEC + ITKSUM * KCQVEC + J
      J1(J) = KOQVEC + ITK1   * KCQVEC + J
      J2(J) = KOQVEC + ITK2   * KCQVEC + J
      JS(J) = KOQVEC + J * KCQVEC + JQVESP
C----------------------------------------------------------------------
C       basic track attributes :
C
      RW(JR(JQVEQX)) = RW(J1(JQVEQX)) + RW(J2(JQVEQX))
      RW(JR(JQVEQY)) = RW(J1(JQVEQY)) + RW(J2(JQVEQY))
      RW(JR(JQVEQZ)) = RW(J1(JQVEQZ)) + RW(J2(JQVEQZ))
      RW(JR(JQVEQE)) = RW(J1(JQVEQE)) + RW(J2(JQVEQE))
      RW(JR(JQVEQP)) = SQRT (RW(JR(JQVEQX))**2 + RW(JR(JQVEQY))**2 +
     &   RW(JR(JQVEQZ))**2)
      RW(JR(JQVEQM)) = SQRT (AMAX1((RW(JR(JQVEQE)) + RW(JR(JQVEQP))) *
     &  (RW(JR(JQVEQE)) - RW(JR(JQVEQP))), 0.))
      RW(JR(JQVECH)) = RW(J1(JQVECH)) + RW(J2(JQVECH))
C
      IF (ITKSUM .LE. KLUST .OR. ITK1 .LE. KLUST .OR.
     &  ITK2 .LE. KLUST)  GO TO 90
C
C       particle code, class :
C
      IW(JR(JQVEPA)) = 0
      ICLAS1 = IW(J1(JQVECL))
      ICLAS2 = IW(J2(JQVECL))
      IF ( ICLAS1 .NE. ICLAS2 ) CALL QMTERM
     &    ('_QVADD2_ Particles from different classes mixed up')
      IW(JR(JQVECL)) = ICLAS1
C
C       KSAME pointer :
C
      ITKS0 = IW(JS(ITKSUM))
      IF (ITKS0 .NE. ITKSUM)  THEN
        IW(JR(JQVEND)) = 0
        ITKS1 = ITKS0
   10   ITKS2 = IW(JS(ITKS1))
        IF (ITKS2 .NE. ITKSUM)  THEN
          ITKS1 = ITKS2
          GO TO 10
        ENDIF
        IW(JS(ITKS1)) = ITKS0
      ENDIF
      IW(JS(ITKSUM)) = ITKSUM
C
C       mother - daughter - mother relation; match :
C
      IF (IW(JR(JQVEND)) .GE. 2)  THEN
        IW(KOQLIN+IW(JR(JQVEDL))+1) = ITK1
        IW(KOQLIN+IW(JR(JQVEDL))+2) = ITK2
      ELSE
        IF (KNQLIN+2 .GE. IW(KOQLIN))  CALL QSBANK ('QLIN', KNQLIN+500)
        IW(KOQLIN+KNQLIN+1) = ITK1
        IW(KOQLIN+KNQLIN+2) = ITK2
        IW(JR(JQVEDL)) = KNQLIN
        KNQLIN = KNQLIN + 2
      ENDIF
      IW(JR(JQVEND)) = 2
      IW(JR(JQVENO)) = 0
      IW(JR(JQVENM)) = 0
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
C       error matrix :
C
      IF (RW(J1(JQVEEM)) .GT. 0. .AND. RW(J2(JQVEEM)) .GT. 0.)  THEN
        RW(JR(JQVEEM  )) = RW(J1(JQVEEM  )) + RW(J2(JQVEEM  ))
        RW(JR(JQVEEM+1)) = RW(J1(JQVEEM+1)) + RW(J2(JQVEEM+1))
        RW(JR(JQVEEM+2)) = RW(J1(JQVEEM+2)) + RW(J2(JQVEEM+2))
        RW(JR(JQVEEM+3)) = RW(J1(JQVEEM+3)) + RW(J2(JQVEEM+3))
        RW(JR(JQVEEM+4)) = RW(J1(JQVEEM+4)) + RW(J2(JQVEEM+4))
        RW(JR(JQVEEM+5)) = RW(J1(JQVEEM+5)) + RW(J2(JQVEEM+5))
        RW(JR(JQVEEM+6)) = RW(J1(JQVEEM+6)) + RW(J2(JQVEEM+6))
        RW(JR(JQVEEM+7)) = RW(J1(JQVEEM+7)) + RW(J2(JQVEEM+7))
        RW(JR(JQVEEM+8)) = RW(J1(JQVEEM+8)) + RW(J2(JQVEEM+8))
        RW(JR(JQVEEM+9)) = RW(J1(JQVEEM+9)) + RW(J2(JQVEEM+9))
      ELSE
        RW(JR(JQVEEM)) = -1.
      ENDIF
      RW(JR(JQVECF)) = -1.
C
   90 IF (ITKSUM .GT. KLUST .AND. ITKSUM .LE. KLMCT)  THEN
        IERR = IERR + 1
        IF (IERR .LE. 10)  CALL QWMESE
     &    ('_QVADD2_ Warning : Unviolable particle has been modified')
      ENDIF
C
      END
#endif
