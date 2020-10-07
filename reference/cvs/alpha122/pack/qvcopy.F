      SUBROUTINE QVCOPY (ITKTO,ITKFRO)
CKEY VECTOR KINEMATICS COPY/USER
C----------------------------------------------------------------------
C! Copy track ITKFRO to ITKTO.
C                                                    H.Albrecht  2.1.89
C                                 Modified:          E.Blucher   2.9.92
C Mod. I. Tomalin 9.5.97: So that two copies to same track don't mess up.
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      DATA IERR /0/
C----------------------------------------------------------------------
      JR(J) = KOQVEC + ITKTO  * KCQVEC + J
      J1(J) = KOQVEC + ITKFRO * KCQVEC + J
      JS(J) = KOQVEC + J * KCQVEC + JQVESP
C----------------------------------------------------------------------
C       basic track attributes :
C
      RW(JR(JQVEQX)) = RW(J1(JQVEQX))
      RW(JR(JQVEQY)) = RW(J1(JQVEQY))
      RW(JR(JQVEQZ)) = RW(J1(JQVEQZ))
      RW(JR(JQVEQE)) = RW(J1(JQVEQE))
      RW(JR(JQVEQM)) = RW(J1(JQVEQM))
      RW(JR(JQVEQP)) = RW(J1(JQVEQP))
      RW(JR(JQVECH)) = RW(J1(JQVECH))
C
      IF (ITKFRO .LE. KLUST .OR. ITKTO .LE. KLUST)  GO TO 90
C
C       particle code, class :
C
      IW(JR(JQVEPA)) = 0
      IW(JR(JQVENP)) = 0
      IW(JR(JQVECL)) = IW(J1(JQVECL))
C
C       energy flow weight
C
      IW(JR(JQVEEW)) = IW(J1(JQVEEW))
C
C       KSAME pointer :
C
      ITK = ITKTO
    8 CONTINUE
      NTK = IW(JS(ITK))
      IF (NTK.NE.ITKTO) THEN
        ITK = NTK  
        GOTO 8
      ELSE
        IW(JS(ITK)) = IW(JS(ITKTO))
      END IF
C
      IW(JS(ITKTO)) = IW(JS(ITKFRO))
      IW(JS(ITKFRO)) = ITKTO
C
C       mother - daughter - mother relation, match :
C
      IW(JR(JQVEND)) = IW(J1(JQVEND))
      IW(JR(JQVEDL)) = IW(J1(JQVEDL))
      IW(JR(JQVENO)) = 0
      IW(JR(JQVENM)) = IW(J1(JQVENM))
      IW(JR(JQVEML)) = IW(J1(JQVEML))
C
C       bit masks :
C
      DO 9 IB=1,KLOCKM
        IW(JR(JQVEBM+IB-1)) = IW(J1(JQVEBM+IB-1))
 9     CONTINUE
      IW(JR(JQVELK)) = IW(J1(JQVELK))
C
C       vertices :
C
      IW(JR(JQVESC)) = IW(J1(JQVESC))
      IW(JR(JQVEOV)) = IW(J1(JQVEOV))
      IW(JR(JQVEEV)) = IW(J1(JQVEEV))
      RW(JR(JQVEDB)) = RW(J1(JQVEDB))
      RW(JR(JQVEZB)) = RW(J1(JQVEZB))
      RW(JR(JQVESD)) = RW(J1(JQVESD))
      RW(JR(JQVESZ)) = RW(J1(JQVESZ))
      RW(JR(JQVECB)) = RW(J1(JQVECB))
C
C       other pointers etc.
C
      IW(JR(JQVETN)) = IW(J1(JQVETN))
      IW(JR(JQVEQD)) = IW(J1(JQVEQD))
      IW(JR(JQVEKS)) = IW(J1(JQVEKS))
C
C        error matrix :
C
      RW(JR(JQVEEM)) = RW(J1(JQVEEM))
      IF (RW(JR(JQVEEM)) .GE. 0.)  THEN
        RW(JR(JQVEEM+1)) = RW(J1(JQVEEM+1))
        RW(JR(JQVEEM+2)) = RW(J1(JQVEEM+2))
        RW(JR(JQVEEM+3)) = RW(J1(JQVEEM+3))
        RW(JR(JQVEEM+4)) = RW(J1(JQVEEM+4))
        RW(JR(JQVEEM+5)) = RW(J1(JQVEEM+5))
        RW(JR(JQVEEM+6)) = RW(J1(JQVEEM+6))
        RW(JR(JQVEEM+7)) = RW(J1(JQVEEM+7))
        RW(JR(JQVEEM+8)) = RW(J1(JQVEEM+8))
        RW(JR(JQVEEM+9)) = RW(J1(JQVEEM+9))
      ENDIF
C
C     Copy user flags
C
      DO 10 I=1,KCQVEC-JQVEUS+1
         RW(JR(JQVEUS+I-1))=RW(J1(JQVEUS+I-1))
10    CONTINUE
      RW(JR(JQVECF)) = RW(J1(JQVECF))
C
   90 IF (KFFILL .GT. 0 .AND.
     &   ITKTO .GT. KLUST .AND. ITKTO .LE. KLMCT)  THEN
        IERR = IERR + 1
        IF (IERR .LE. 10)  CALL QWMESE
     &    ('_QVCOPY_ Warning : Unviolable particle is modified')
      ENDIF
C
      END
#endif
