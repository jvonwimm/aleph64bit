      FUNCTION KVFITM (ITK,CNAM,IER)
CKEY PART /USER
C----------------------------------------------------------------------
C! Fit mass and save particle with name CNAM
C                                                    H.Albrecht 9.11.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE NER
#include "qcdesh.h"
      DOUBLE PRECISION AM2,E,PX,PY,PZ,SXX,SXY,SYY,SXZ,SYZ,SZZ,
     +  SXE,SYE,SZE,SEE,AX,AY,AZ,AE,CS,GL,FU,FO,FA,DX,DY,DZ,DE
      CHARACTER * (*) CNAM
      DATA NER /0/
C----------------------------------------------------------------------
      JR(J) = KOQVEC + KVFITM * KCQVEC + J
      J1(J) = KOQVEC + ITK    * KCQVEC + J
C----------------------------------------------------------------------
      ICLASS = IW(J1(JQVECL))
      IPC = KPART (CNAM)
      IF (IPC .EQ. 0)  THEN
        NER = NER + 1
        IF (NER .LE. 10)  CALL QWMESE
     +    ('_KVFITM_ No particle name given')
        IER = 1
        GO TO 80
      ENDIF
C
      AM1 = RW(KOQPAR+IPC*KCQPAR+JQPAMA)
      AM2 = AM1**2
C       converted photons get 1 MeV (avoid E < p due to rounding errors)
      IF (AM2 .EQ. 0.)  AM2 = 1E-6
      ITERAT = 0
C
C       measured quantities
C
      E = RW(J1(JQVEQE))
      PX = RW(J1(JQVEQX))
      PY = RW(J1(JQVEQY))
      PZ = RW(J1(JQVEQZ))
      SXX = RW(J1(JQVEEM  ))
      IF (SXX .LE. 0. .OR. ICLASS .EQ. 0)  THEN
        NER = NER + 1
        IF (NER .LE. 10)  CALL QWMESE
     +    ('_KVFITM_ Covariance matrix not available')
        IER = 2
        GO TO 80
      ENDIF
      SXY = RW(J1(JQVEEM+1))
      SYY = RW(J1(JQVEEM+2))
      SXZ = RW(J1(JQVEEM+3))
      SYZ = RW(J1(JQVEEM+4))
      SZZ = RW(J1(JQVEEM+5))
      SXE = RW(J1(JQVEEM+6))
      SYE = RW(J1(JQVEEM+7))
      SZE = RW(J1(JQVEEM+8))
      SEE = RW(J1(JQVEEM+9))
      FU = AM2 + PX**2 + PY**2 + PZ**2 - E**2
      FO = 1.E30
C
C      iteration
C
 10   AX = 2. * (E * SXE - PX * SXX - PY * SXY - PZ * SXZ)
      AY = 2. * (E * SYE - PX * SXY - PY * SYY - PZ * SYZ)
      AZ = 2. * (E * SZE - PX * SXZ - PY * SYZ - PZ * SZZ)
      AE = 2. * (E * SEE - PX * SXE - PY * SYE - PZ * SZE)
C
      CS = 2. * (E * AE - PX * AX - PY * AY - PZ * AZ)
      IF (CS .LT. 1E-12)  THEN
        NER = NER + 1
        IF (NER .LE. 10)  CALL QWMESE  ('_KVFITM_ Singular matrix')
        IER = 3
        GO TO 80
      ENDIF
      CS = 1. / CS
C
      IF (ITERAT .EQ. 0)  THEN
        GL = CS * FU
      ELSE
        GL = CS * (2. * (E * DE - PX * DX - PY * DY - PZ * DZ) + FU)
      ENDIF
      DE = AE * GL
      DX = AX * GL
      DY = AY * GL
      DZ = AZ * GL
C
C       fitted quantities
C
      E = RW(J1(JQVEQE)) + DE
      PX = RW(J1(JQVEQX)) + DX
      PY = RW(J1(JQVEQY)) + DY
      PZ = RW(J1(JQVEQZ)) + DZ
      FU = AM2 + PX**2 + PY**2 + PZ**2 - E**2
C
C       convergence ?
C
      FA = ABS (FU)
      IF (FA .LE. 1E-6)  GO TO 30
      IF (FA .GT. FO + 1E-3)  THEN
        NER = NER + 1
        IF (NER .LE. 10)  CALL QWMESE  ('_KVFITM_ Chi2 increasing')
        IER = 4
        GO TO 80
      ENDIF
      FV = FO
      FO = FA
      ITERAT = ITERAT + 1
      IF (ITERAT .LE. 10)  GO TO 10
        NER = NER + 1
        IF (NER .LE. 10)  CALL QWMESE  ('_KVFITM_ Too many iterations')
      IER = 5
      GO TO 80
C
C       new track number :
C
 30   IER  = 0
      IF (KTDROP .NE. 0)  THEN
        KVFITM = KTDROP
        KTDROP = IW(JR(JQVENP))
      ELSE
        KVFITM = KFFRT
        KFFRT = KFFRT + 1
        IF (KFFRT .GE. KLFRT)  CALL QSBANK ('QVEC', KFFRT+200)
      ENDIF
C
C       basic track attributes :
C
      RW(JR(JQVEQX)) = PX
      RW(JR(JQVEQY)) = PY
      RW(JR(JQVEQZ)) = PZ
      RW(JR(JQVEQE)) = E
      RW(JR(JQVEQM)) = RW(J1(JQVEQM))
      RW(JR(JQVEQP)) = SQRT (PX**2 + PY**2 + PZ**2)
      RW(JR(JQVECH)) = RW(J1(JQVECH))
C
C        error matrix :
C
      RW(JR(JQVEEM  )) = RW(J1(JQVEEM  )) - AX * AX * CS
      RW(JR(JQVEEM+1)) = RW(J1(JQVEEM+1)) - AX * AY * CS
      RW(JR(JQVEEM+2)) = RW(J1(JQVEEM+2)) - AY * AY * CS
      RW(JR(JQVEEM+3)) = RW(J1(JQVEEM+3)) - AX * AZ * CS
      RW(JR(JQVEEM+4)) = RW(J1(JQVEEM+4)) - AY * AZ * CS
      RW(JR(JQVEEM+5)) = RW(J1(JQVEEM+5)) - AZ * AZ * CS
      RW(JR(JQVEEM+6)) = RW(J1(JQVEEM+6)) - AX * AE * CS
      RW(JR(JQVEEM+7)) = RW(J1(JQVEEM+7)) - AY * AE * CS
      RW(JR(JQVEEM+8)) = RW(J1(JQVEEM+8)) - AZ * AE * CS
      RW(JR(JQVEEM+9)) = RW(J1(JQVEEM+9)) - AE * AE * CS
      RW(JR(JQVECF)) = QMCHI2 (ITK, CNAM)
      IF (RW(JR(JQVEEM  )) .LE. 0. .OR. RW(JR(JQVEEM+2)) .LE. 0. .OR.
     +    RW(JR(JQVEEM+5)) .LE. 0. .OR. RW(JR(JQVEEM+9)) .LE. 0.)  THEN
        NER = NER + 1
        IF (NER .LE. 10)  CALL QWMESE
     +    ('_KVFITM_ Inconsistency in fitted cov. matrix')
        IER = 6
        GO TO 80
      ENDIF
C
C       particle code, class :
C       daughter - mother relation of daughters :
C
      IW(JR(JQVECL)) = ICLASS
      IW(JR(JQVEPA)) = IPC
      IFP = IW(KOQPLI+IPC)
      IF (IFP .EQ. 0)  IFP = KFPADR (IPC)
      IW(JR(JQVENP)) = IW(KOQFPA+IFP*KCQFPA+ICLASS)
      IW(KOQFPA+IFP*KCQFPA+ICLASS) = KVFITM
      IF (IW(J1(JQVEND)) .NE. 0)  CALL QAMOTH
     &  (KVFITM, IW(J1(JQVEND)), IW(KOQLIN+IW(J1(JQVEDL))+1))
C
C       KSAME pointer :
C
      IW(JR(JQVESP)) = IW(J1(JQVESP))
      IW(J1(JQVESP)) = KVFITM
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
C       normal return
C
      IER = 0
      GO TO 90
C
C       error condition
C
 80   KVFITM = ITK
 90   RETURN
      END
#endif
