      FUNCTION KIDSAV (ITK,CNAM)
CKEY PART /USER
C----------------------------------------------------------------------
C!  save particle with name cnam and set the mass
C                                                   H.ALBRECHT 19.10.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      CHARACTER * (*) CNAM
      DATA IERR /0/
C-----------------------------------------------------------------------
      JR(J) = KOQVEC + KIDSAV * KCQVEC + J
      J1(J) = KOQVEC + ITK    * KCQVEC + J
C-----------------------------------------------------------------------
C       new track number :
C
      IF (KTDROP .NE. 0)  THEN
        KIDSAV = KTDROP
        KTDROP = IW(JR(JQVENP))
      ELSE
        KIDSAV = KFFRT
        KFFRT = KFFRT + 1
        IF (KFFRT .GE. KLFRT)  CALL QSBANK ('QVEC', KFFRT+200)
      ENDIF
C
C       basic track attributes :
C
      PX = RW(J1(JQVEQX))
      RW(JR(JQVEQX)) = PX
      PY = RW(J1(JQVEQY))
      RW(JR(JQVEQY)) = PY
      PZ = RW(J1(JQVEQZ))
      RW(JR(JQVEQZ)) = PZ
      P = RW(J1(JQVEQP))
      RW(JR(JQVEQP)) = P
      CH = RW(J1(JQVECH))
C
C       particle code, class :
C       daughter - mother relation of daughters :
C
      ICLASS = IW(J1(JQVECL))
      IW(JR(JQVECL)) = ICLASS
      IF (CNAM .NE. ' ')  THEN
        IPC = KPART (CNAM)
        CHN = RW(KOQPAR+IPC*KCQPAR+JQPACH)
        IF (CH * CHN)  10,20,30
 10     IPC = IW(KOQPAR+IPC*KCQPAR+JQPAAN)
        CHN = RW(KOQPAR+IPC*KCQPAR+JQPACH)
        GO TO 30
 20     IF (CH + CHN .NE. 0.)  THEN
          IERR = IERR + 1
          IF (IERR .LE. 10)  CALL QWMESE
     &         ('_KIDSAV_ wrong charge')
        ENDIF
 30     RW(JR(JQVECH)) = CHN
        AM = RW(KOQPAR+IPC*KCQPAR+JQPAMA)
        RW(JR(JQVEQM)) = AM
        E = SQRT (AM**2 + P**2)
        RW(JR(JQVEQE)) = E
        IW(JR(JQVEPA)) = IPC
        IF (ICLASS .EQ. 0)  GO TO 40
        IFP = IW(KOQPLI+IPC)
        IF (IFP .EQ. 0)  IFP = KFPADR (IPC)
        IW(JR(JQVENP)) = IW(KOQFPA+IFP*KCQFPA+ICLASS)
        IW(KOQFPA+IFP*KCQFPA+ICLASS) = KIDSAV
        IF (IW(J1(JQVEND)) .NE. 0)  CALL QAMOTH
     &    (KIDSAV, IW(J1(JQVEND)), IW(KOQLIN+IW(J1(JQVEDL))+1))
        GO TO 50
      ELSE
        RW(JR(JQVEQM)) = 0.
        RW(JR(JQVEQE)) = P
        IW(JR(JQVENP)) = 0
        IW(JR(JQVEPA)) = 0
      ENDIF
C
C       class = 0 --> zero attributes
C
   40 IERR = IERR + 1
      IF (IERR .LE. 10)  CALL QWMESE
     &  ('_KIDSAV_ No particle name given')
C
C       KSAME pointer :
C
   50 IW(JR(JQVESP)) = IW(J1(JQVESP))
      IW(J1(JQVESP)) = KIDSAV
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
        SXX = RW(J1(JQVEEM))
        RW(JR(JQVEEM)) = SXX
        SXY = RW(J1(JQVEEM+1))
        RW(JR(JQVEEM+1)) = SXY
        SYY = RW(J1(JQVEEM+2))
        RW(JR(JQVEEM+2)) = SYY
        SXZ = RW(J1(JQVEEM+3))
        RW(JR(JQVEEM+3)) = SXZ
        SYZ = RW(J1(JQVEEM+4))
        RW(JR(JQVEEM+4)) = SYZ
        SZZ = RW(J1(JQVEEM+5))
        RW(JR(JQVEEM+5)) = SZZ
        RW(JR(JQVEEM+6)) = (PX*SXX + PY*SXY + PZ*SXZ) / E
        RW(JR(JQVEEM+7)) = (PX*SXY + PY*SYY + PZ*SYZ) / E
        RW(JR(JQVEEM+8)) = (PX*SXZ + PY*SYZ + PZ*SZZ) / E
        RW(JR(JQVEEM+9)) = (PX**2*SXX + PY**2*SYY + PZ**2*SZZ + 2. *
     +    (PX * (PY*SXY + PZ * SXZ) + PY*PZ*SYZ)) / E**2
      ELSE
        RW(JR(JQVEEM)) = -1.
      ENDIF
      RW(JR(JQVECF)) = -1.
C
      END
#endif
