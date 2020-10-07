      FUNCTION KVSAVA (ITK,CNAM,IANTI)
CKEY PART /USER
C----------------------------------------------------------------------
C! Save particle with name CNAM or with antipart (CNAME)
C                                                    H.Albrecht  2.1.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      CHARACTER * (*) CNAM
      DATA IERR /0/
      JR(J) = KOQVEC + KVSAVA * KCQVEC + J
      J1(J) = KOQVEC + ITK    * KCQVEC + J
C
C       new track number :
C
      IF (KTDROP .NE. 0)  THEN
        KVSAVA = KTDROP
        KTDROP = IW(JR(JQVENP))
      ELSE
        KVSAVA = KFFRT
        KFFRT = KFFRT + 1
        IF (KFFRT .GE. KLFRT)  CALL QSBANK ('QVEC', KFFRT+200)
      ENDIF
C
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
C       particle code, class :
C       daughter - mother relation of daughters :
C
      ICLASS = IW(J1(JQVECL))
      IW(JR(JQVECL)) = ICLASS
      IF (CNAM .NE. ' ')  THEN
        IPC = KPART (CNAM)
        IF (IANTI .NE. 0)  IPC = IW(KOQPAR+IPC*KCQPAR+JQPAAN)
        IW(JR(JQVEPA)) = IPC
        IF (ICLASS .EQ. 0)  GO TO 10
        IFP = IW(KOQPLI+IPC)
        IF (IFP .EQ. 0)  IFP = KFPADR (IPC)
        IW(JR(JQVENP)) = IW(KOQFPA+IFP*KCQFPA+ICLASS)
        IW(KOQFPA+IFP*KCQFPA+ICLASS) = KVSAVA
        IF (IW(J1(JQVEND)) .NE. 0)  CALL QAMOTH
     &    (KVSAVA, IW(J1(JQVEND)), IW(KOQLIN+IW(J1(JQVEDL))+1))
      ELSE
        IW(JR(JQVEPA)) = 0
        IF (ICLASS .EQ. 0)  GO TO 10
      ENDIF
      GO TO 20
C
C       class = 0 --> zero attributes
C
   10 IERR = IERR + 1
      IF (IERR .LE. 10)  CALL QWMESE
     &  ('_KVSAVA_ Particle to be saved has zero attributes')
      IW(JR(JQVENP)) = 0
C
C       KSAME pointer :
C
   20 IW(JR(JQVESP)) = IW(J1(JQVESP))
      IW(J1(JQVESP)) = KVSAVA
C
C       mother - daughter relation, match :
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
 9    CONTINUE
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
      IF (RW(J1(JQVEEM)) .GE. 0.)  THEN
        RW(JR(JQVEEM)) = RW(J1(JQVEEM))
        RW(JR(JQVEEM+1)) = RW(J1(JQVEEM+1))
        RW(JR(JQVEEM+2)) = RW(J1(JQVEEM+2))
        RW(JR(JQVEEM+3)) = RW(J1(JQVEEM+3))
        RW(JR(JQVEEM+4)) = RW(J1(JQVEEM+4))
        RW(JR(JQVEEM+5)) = RW(J1(JQVEEM+5))
        RW(JR(JQVEEM+6)) = RW(J1(JQVEEM+6))
        RW(JR(JQVEEM+7)) = RW(J1(JQVEEM+7))
        RW(JR(JQVEEM+8)) = RW(J1(JQVEEM+8))
        RW(JR(JQVEEM+9)) = RW(J1(JQVEEM+9))
      ELSE
        RW(JR(JQVEEM)) = -1.
      ENDIF
      RW(JR(JQVECF)) = RW(J1(JQVECF))
C
      END
#endif
