      SUBROUTINE QFCOEQ(EN,TH,PH,SIGEN,SIGPH,SIGTH,NTK1,ICALFL)
CKEY FILL CAL /INTERNAL
C----------------------------------------------------------------------
C! Fill QVEC for calorimeter objects
C
C  auxiliary to  QFCOBJ
C                                                    J.Boucrot  23.9.93
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "qcde.h"
C----------------------------------------------------------------------
      JV1(I) = KOQVEC + NTK1 * KCQVEC + I
      JD1(I) = JQDET + NTK1 * KCQDET + I
C----------------------------------------------------------------------
      CP = COS (PH)
      SP = SIN (PH)
      CT = COS (TH)
      ST = SIN (TH)
      PT = EN * ST
      PX = PT * CP
      RW(JV1(JQVEQX)) = PX
      PY = PT * SP
      RW(JV1(JQVEQY)) = PY
      PZ = EN * CT
      RW(JV1(JQVEQZ)) = PZ
      RW(JV1(JQVEQP)) = EN
      RW(JV1(JQVEQE)) = EN
      RW(JV1(JQVEQM)) = 0.
      RW(JV1(JQVECH)) = 0.
C
C         error matrix :
C
      SXX = (ST * CP)**2 * SIGEN + EN**2 *
     &  ((CT * CP)**2 * SIGTH + (ST * SP)**2 * SIGPH)
      RW(JV1(JQVEEM)) = SXX
      SXY = CP * SP * (ST**2 * SIGEN + (EN * CT)**2 * SIGTH -
     &  (EN * ST)**2 * SIGPH)
      RW(JV1(JQVEEM+1)) = SXY
      SYY = (ST * SP)**2 * SIGEN + EN**2 *
     &  ((CT * SP)**2 * SIGTH + (ST * CP)**2 * SIGPH)
      RW(JV1(JQVEEM+2)) = SYY
      SXZ = ST * (CT * CP * SIGEN + EN**2 * CP * SIGTH)
      RW(JV1(JQVEEM+3)) = SXZ
      SYZ = ST * (CT * SP * SIGEN + EN**2 * SP * SIGTH)
      RW(JV1(JQVEEM+4)) = SYZ
      SZZ = CT**2 * SIGEN + (EN * ST)**2 * SIGTH
      RW(JV1(JQVEEM+5)) = SZZ
      IF(EN.EQ.0.)GOTO 701
C
      RW(JV1(JQVEEM+6)) = (PX * SXX + PY * SXY + PZ * SXZ) / EN
      RW(JV1(JQVEEM+7)) = (PX * SXY + PY * SYY + PZ * SYZ) / EN
      RW(JV1(JQVEEM+8)) = (PX * SXZ + PY * SYZ + PZ * SZZ) / EN
      RW(JV1(JQVEEM+9)) = (PX**2 * SXX + PY**2 * SYY + PZ**2 * SZZ +
     &  2. * (PX * (PY * SXY + PZ * SXZ) + PY * PZ * SYZ)) / EN**2
  701 CONTINUE
      RW(JV1(JQVECF)) = -1.
C
C         bank QVEC :
C
      JQDET = KOQDET - KLUST * KCQDET
      IW(JV1(JQVESC)) = 1
      IW(JV1(JQVEKS)) = 0
      IW(JV1(JQVECL)) = 1
      IW(JV1(JQVEQD)) = JD1 (0)
      IW(JV1(JQVESP)) = NTK1
      IW(JV1(JQVEOV)) = 0
      IW(JV1(JQVEEV)) = 0
      IW(JV1(JQVEND)) = 0
      IW(JV1(JQVENO)) = 0
      IW(JV1(JQVENM)) = 0
      DO 9 IB=1,KLOCKM
      IW(JV1(JQVEBM+IB-1)) = 0
 9    CONTINUE
      IW(JV1(JQVELK)) = 0
      RW(JV1(JQVEDB)) = 0.
      RW(JV1(JQVEZB)) = 0.
      RW(JV1(JQVESD)) = 0.
      RW(JV1(JQVESZ)) = 0.
      RW(JV1(JQVECB)) = 0.
      RW(JV1(JQVEEW)) = 0.
C
C         bank QDET :
C
      IW(JD1(JQDEAF)) = KQZER
      IW(JD1(JQDEAL)) = KQZER
      IW(JD1(JQDENT)) = 0
      IW(JD1(JQDEAT)) = KQZER
      IW(JD1(JQDEAE)) = KQZER
      IW(JD1(JQDEAH)) = KQZER
      IW(JD1(JQDEAM)) = KQZER
      IW(JD1(JQDEMU)) = KQZER
      IW(JD1(JQDECF)) = ICALFL
      IW(JD1(JQDENF)) = 0
C
 999  END
#endif
