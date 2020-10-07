      SUBROUTINE QVSUB (ITKRES,ITKSUM,ITK2)
CKEY VECTOR KINEMATICS /USER
C----------------------------------------------------------------------
C! 4-vector(ITKSUM) - 4-vector(ITK1) --> 4-vector(ITKRES)
C                                                    H.Albrecht  2.1.89
C                                     Modified:      E. Blucher  2.2.92
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      DATA IERR /0/
C----------------------------------------------------------------------
      JR(J) = KOQVEC + ITKRES * KCQVEC + J
      J1(J) = KOQVEC + ITKSUM * KCQVEC + J
      J2(J) = KOQVEC + ITK2   * KCQVEC + J
      JS(J) = KOQVEC + J * KCQVEC + JQVESP
C----------------------------------------------------------------------
C       basic track attributes :
C
      RW(JR(JQVEQX)) = RW(J1(JQVEQX)) - RW(J2(JQVEQX))
      RW(JR(JQVEQY)) = RW(J1(JQVEQY)) - RW(J2(JQVEQY))
      RW(JR(JQVEQZ)) = RW(J1(JQVEQZ)) - RW(J2(JQVEQZ))
      RW(JR(JQVEQP)) = SQRT (RW(JR(JQVEQX))**2 + RW(JR(JQVEQY))**2 +
     &   RW(JR(JQVEQZ))**2)
      RW(JR(JQVEQE)) = RW(J1(JQVEQE)) - RW(J2(JQVEQE))
      IF (RW(JR(JQVEQE)) .GT. 0.)  THEN
        RW(JR(JQVEQM)) = (RW(JR(JQVEQE)) + RW(JR(JQVEQP))) *
     &                   (RW(JR(JQVEQE)) - RW(JR(JQVEQP)))
        IF (RW(JR(JQVEQM)) .GE. 0.)  THEN
          RW(JR(JQVEQM)) = SQRT (RW(JR(JQVEQM)))
        ELSE
          RW(JR(JQVEQM)) = - SQRT (-RW(JR(JQVEQM)))
          IERR = IERR + 1
          IF (IERR .LE. 10)
     &      CALL QWMESS ('_QVSUB_ Result has negative mass**2')
        ENDIF
      ELSE
        RW(JR(JQVEQE)) = 0.
        RW(JR(JQVEQM)) = 0.
        IERR = IERR + 1
        IF (IERR .LE. 10)
     &    CALL QWMESS ('_QVSUB_ Result has negative energy')
      ENDIF
      RW(JR(JQVECH)) = RW(J1(JQVECH)) - RW(J2(JQVECH))
C
      IF (ITKRES .LE. KLUST .OR. ITKSUM .LE. KLUST .OR.
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
     &    ('_QVSUB_ Particles from different classes mixed up')
C
C       mother - daughter - mother relation, match :
C
      IW(JR(JQVEND)) = 0
      IW(JR(JQVENO)) = 0
      IW(JR(JQVENM)) = 0
C
C       bit masks :
C
      DO 9 IB=1,KLOCKM
      IW(JR(JQVEBM+IB-1))
     &= IAND (IW(J1(JQVEBM+IB-1)), NOT(IW(J2(JQVEBM+IB-1))))
9     CONTINUE
C
C       error matrix :
C
      RW(JR(JQVEEM)) = -1.
      RW(JR(JQVECF)) = -1.
C
   90 IF (ITKRES .GT. KLUST .AND. ITKRES .LE. KLMCT)  THEN
        IERR = IERR + 1
        IF (IERR .LE. 10)  CALL QWMESE
     &    ('_QVSUB_ Warning: Unviolable particle has been modified')
      ENDIF
C
      END
#endif
