      SUBROUTINE QVADDN (ITKSUM,ITK)
CKEY VECTOR KINEMATICS /USER
C----------------------------------------------------------------------
C! 4-vector(ITKSUM) + 4-vector(ITK) --> 4-vector(ITKSUM)
C                                                    H.Albrecht  2.1.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      DATA IERR /0/
C----------------------------------------------------------------------
      JR(J) = KOQVEC + ITKSUM * KCQVEC + J
      J1(J) = KOQVEC + ITK    * KCQVEC + J
C----------------------------------------------------------------------
C       basic track attributes :
C
      RW(JR(JQVEQX)) = RW(JR(JQVEQX)) + RW(J1(JQVEQX))
      RW(JR(JQVEQY)) = RW(JR(JQVEQY)) + RW(J1(JQVEQY))
      RW(JR(JQVEQZ)) = RW(JR(JQVEQZ)) + RW(J1(JQVEQZ))
      RW(JR(JQVEQE)) = RW(JR(JQVEQE)) + RW(J1(JQVEQE))
      RW(JR(JQVEQP)) = SQRT (RW(JR(JQVEQX))**2 + RW(JR(JQVEQY))**2 +
     &   RW(JR(JQVEQZ))**2)
      RW(JR(JQVEQM)) = SQRT (AMAX1((RW(JR(JQVEQE)) + RW(JR(JQVEQP))) *
     &  (RW(JR(JQVEQE)) - RW(JR(JQVEQP))), 0.))
      RW(JR(JQVECH)) = RW(JR(JQVECH)) + RW(J1(JQVECH))
C
      IF (ITKSUM .LE. KLUST .OR. ITK .LE. KLUST)  GO TO 90
C
C       particle code : set in KVNEW
C       class :
C
      ICLASS = IW(JR(JQVECL))
      IF (ICLASS .EQ. 0)  THEN
        ICLASS = IW(J1(JQVECL))
        IW(JR(JQVECL)) = ICLASS
C
C         KSAME pointer : set in KVNEW
C         vertices : set in KVNEW except :
C
        IW(JR(JQVESC)) = -1
        IW(JR(JQVEOV)) = IW(J1(JQVEOV))
        IW(JR(JQVEEV)) = IW(J1(JQVEOV))
      ENDIF
C
      IF (ICLASS .NE. IW(J1(JQVECL)))  CALL QMTERM
     &    ('_QVADDN_ Particles from different classes mixed up')
C
C       other pointers etc. : set in KVNEW
C       daughter - mother relation, match : set in KVNEW.
C       mother - daughter relation :
C
      CALL QADAU (ITKSUM, 1, ITK)
C
C       bit masks :
C
      DO 9 IB=1,KLOCKM
      IW(JR(JQVEBM+IB-1))
     &= IOR (IW(JR(JQVEBM+IB-1)),IW(J1(JQVEBM+IB-1)))
    9 CONTINUE
      IW(JR(JQVELK)) = 0
C
C       error matrix :
C
      IF (RW(JR(JQVEEM)) .GE. 0. .AND. RW(J1(JQVEEM)) .GE. 0.)  THEN
        RW(JR(JQVEEM  )) = RW(JR(JQVEEM  )) + RW(J1(JQVEEM  ))
        RW(JR(JQVEEM+1)) = RW(JR(JQVEEM+1)) + RW(J1(JQVEEM+1))
        RW(JR(JQVEEM+2)) = RW(JR(JQVEEM+2)) + RW(J1(JQVEEM+2))
        RW(JR(JQVEEM+3)) = RW(JR(JQVEEM+3)) + RW(J1(JQVEEM+3))
        RW(JR(JQVEEM+4)) = RW(JR(JQVEEM+4)) + RW(J1(JQVEEM+4))
        RW(JR(JQVEEM+5)) = RW(JR(JQVEEM+5)) + RW(J1(JQVEEM+5))
        RW(JR(JQVEEM+6)) = RW(JR(JQVEEM+6)) + RW(J1(JQVEEM+6))
        RW(JR(JQVEEM+7)) = RW(JR(JQVEEM+7)) + RW(J1(JQVEEM+7))
        RW(JR(JQVEEM+8)) = RW(JR(JQVEEM+8)) + RW(J1(JQVEEM+8))
        RW(JR(JQVEEM+9)) = RW(JR(JQVEEM+9)) + RW(J1(JQVEEM+9))
      ELSE
        RW(JR(JQVEEM)) = -1.
      ENDIF
      RW(JR(JQVECF)) = -1.
C
   90 IF (ITKSUM .GT. KLUST .AND. ITKSUM .LE. KLMCT)  THEN
        IERR = IERR + 1
        IF (IERR .LE. 10)  CALL QWMESE
     &    ('_QVADDN_ Warning : Unviolable particle has been modified')
      ENDIF
C
      END
#endif
