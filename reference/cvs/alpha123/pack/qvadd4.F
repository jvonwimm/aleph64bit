      SUBROUTINE QVADD4 (ITKSUM,ITK1,ITK2,ITK3,ITK4)
CKEY VECTOR KINEMATICS /USER
C----------------------------------------------------------------------
C! Sum (4-vector(ITKn) (n=1,3)) --> 4-vector(ITKSUM)
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
      J3(J) = KOQVEC + ITK3   * KCQVEC + J
      J4(J) = KOQVEC + ITK4   * KCQVEC + J
      JS(J) = KOQVEC + J * KCQVEC + JQVESP
C----------------------------------------------------------------------
C       basic track attributes :
C
      RW(JR(JQVEQX)) = RW(J1(JQVEQX)) + RW(J2(JQVEQX)) +
     &                 RW(J3(JQVEQX)) + RW(J4(JQVEQX))
      RW(JR(JQVEQY)) = RW(J1(JQVEQY)) + RW(J2(JQVEQY)) +
     &                 RW(J3(JQVEQY)) + RW(J4(JQVEQY))
      RW(JR(JQVEQZ)) = RW(J1(JQVEQZ)) + RW(J2(JQVEQZ)) +
     &                 RW(J3(JQVEQZ)) + RW(J4(JQVEQZ))
      RW(JR(JQVEQE)) = RW(J1(JQVEQE)) + RW(J2(JQVEQE)) +
     &                 RW(J3(JQVEQE)) + RW(J4(JQVEQE))
      RW(JR(JQVEQP)) = SQRT (RW(JR(JQVEQX))**2 + RW(JR(JQVEQY))**2 +
     &   RW(JR(JQVEQZ))**2)
      RW(JR(JQVEQM)) = SQRT (AMAX1((RW(JR(JQVEQE)) + RW(JR(JQVEQP))) *
     &  (RW(JR(JQVEQE)) - RW(JR(JQVEQP))), 0.))
      RW(JR(JQVECH)) = RW(J1(JQVECH)) + RW(J2(JQVECH)) +
     &                 RW(J3(JQVECH)) + RW(J4(JQVECH))
C
      IF (ITKSUM .LE. KLUST .OR. ITK1 .LE. KLUST .OR.
     &  ITK2 .LE. KLUST .OR. ITK3 .LE. KLUST .OR.
     &  ITK4 .LE. KLUST)  GO TO 90
C
C       particle code, class :
C
      IW(JR(JQVEPA)) = 0
      ICLASS = IW(J1(JQVECL))
      IF (ICLASS .EQ. 0)  THEN
        ICLASS = IW(J2(JQVECL))
        IF (ICLASS .EQ. 0)  ICLASS = IW(J3(JQVECL))
        IF (ICLASS .EQ. 0)  ICLASS = IW(J4(JQVECL))
      ENDIF
      IW(JR(JQVECL)) = ICLASS
C
      IF (ICLASS .NE. IW(J2(JQVECL)) .OR.
     &    ICLASS .NE. IW(J3(JQVECL)) .OR.
     &    ICLASS .NE. IW(J4(JQVECL)))  CALL QMTERM
     &    ('_QVADD4_ Particles from different classes mixed up')
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
      IF (IW(JR(JQVEND)) .GE. 4)  THEN
        IW(KOQLIN+IW(JR(JQVEDL))+1) = ITK1
        IW(KOQLIN+IW(JR(JQVEDL))+2) = ITK2
        IW(KOQLIN+IW(JR(JQVEDL))+3) = ITK3
        IW(KOQLIN+IW(JR(JQVEDL))+4) = ITK4
      ELSE
        IF (KNQLIN+4 .GE. IW(KOQLIN))  CALL QSBANK ('QLIN', KNQLIN+500)
        IW(KOQLIN+KNQLIN+1) = ITK1
        IW(KOQLIN+KNQLIN+2) = ITK2
        IW(KOQLIN+KNQLIN+3) = ITK3
        IW(KOQLIN+KNQLIN+4) = ITK4
        IW(JR(JQVEDL)) = KNQLIN
        KNQLIN = KNQLIN + 4
      ENDIF
      IW(JR(JQVEND)) = 4
      IW(JR(JQVENO)) = 0
      IW(JR(JQVENM)) = 0
C
C       bit masks :
C
      DO 9 IB=1,KLOCKM
      IW(JR(JQVEBM+IB-1))
     &= IOR (IW(J1(JQVEBM+IB-1)), IOR (IW(J2(JQVEBM+IB-1)),
     &  IOR (IW(J3(JQVEBM+IB-1)),IW(J4(JQVEBM+IB-1)))))
   9  CONTINUE
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
C       error matrix :
C
      IF (RW(J1(JQVEEM)) .GT. 0. .AND. RW(J2(JQVEEM)) .GT. 0. .AND.
     &    RW(J3(JQVEEM)) .GT. 0. .AND. RW(J4(JQVEEM)) .GT. 0.)  THEN
        RW(JR(JQVEEM  )) = RW(J1(JQVEEM  )) + RW(J2(JQVEEM  )) +
     &  RW(J3(JQVEEM  )) + RW(J4(JQVEEM  ))
        RW(JR(JQVEEM+1)) = RW(J1(JQVEEM+1)) + RW(J2(JQVEEM+1)) +
     &  RW(J3(JQVEEM+1)) + RW(J4(JQVEEM+1))
        RW(JR(JQVEEM+2)) = RW(J1(JQVEEM+2)) + RW(J2(JQVEEM+2)) +
     &  RW(J3(JQVEEM+2)) + RW(J4(JQVEEM+2))
        RW(JR(JQVEEM+3)) = RW(J1(JQVEEM+3)) + RW(J2(JQVEEM+3)) +
     &  RW(J3(JQVEEM+3)) + RW(J4(JQVEEM+3))
        RW(JR(JQVEEM+4)) = RW(J1(JQVEEM+4)) + RW(J2(JQVEEM+4)) +
     &  RW(J3(JQVEEM+4)) + RW(J4(JQVEEM+4))
        RW(JR(JQVEEM+5)) = RW(J1(JQVEEM+5)) + RW(J2(JQVEEM+5)) +
     &  RW(J3(JQVEEM+5)) + RW(J4(JQVEEM+5))
        RW(JR(JQVEEM+6)) = RW(J1(JQVEEM+6)) + RW(J2(JQVEEM+6)) +
     &  RW(J3(JQVEEM+6)) + RW(J4(JQVEEM+6))
        RW(JR(JQVEEM+7)) = RW(J1(JQVEEM+7)) + RW(J2(JQVEEM+7)) +
     &  RW(J3(JQVEEM+7)) + RW(J4(JQVEEM+7))
        RW(JR(JQVEEM+8)) = RW(J1(JQVEEM+8)) + RW(J2(JQVEEM+8)) +
     &  RW(J3(JQVEEM+8)) + RW(J4(JQVEEM+8))
        RW(JR(JQVEEM+9)) = RW(J1(JQVEEM+9)) + RW(J2(JQVEEM+9)) +
     &  RW(J3(JQVEEM+9)) + RW(J4(JQVEEM+9))
      ELSE
        RW(JR(JQVEEM)) = -1.
      ENDIF
      RW(JR(JQVECF)) = -1.
C
   90 IF (ITKSUM .GT. KLUST .AND. ITKSUM .LE. KLMCT)  THEN
        IERR = IERR + 1
        IF (IERR .LE. 10)  CALL QWMESE
     &    ('_QVADD4_ Warning : Unviolable particle has been modified')
      ENDIF
C
      END
#endif
