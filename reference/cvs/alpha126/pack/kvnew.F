      FUNCTION KVNEW (DUMMY)
CKEY PART /USER
C----------------------------------------------------------------------
C! Create new momentum=0 track with particle name CNAM
C                                                    H.Albrecht  2.1.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      JR(J) = KOQVEC + KVNEW * KCQVEC + J
C
C       new track number :
C
      IF (KTDROP .NE. 0)  THEN
        KVNEW = KTDROP
        KTDROP = IW(JR(JQVENP))
      ELSE
        KVNEW = KFFRT
        KFFRT = KFFRT + 1
        IF (KFFRT .GE. KLFRT)  CALL QSBANK ('QVEC', KFFRT+200)
      ENDIF
C
C       basic track attributes :
C
      RW(JR(JQVEQX)) = 0.
      RW(JR(JQVEQY)) = 0.
      RW(JR(JQVEQZ)) = 0.
      RW(JR(JQVEQE)) = 0.
      RW(JR(JQVEQM)) = 0.
      RW(JR(JQVEQP)) = 0.
      RW(JR(JQVECH)) = 0.
C
C       particle code, class :
C
      IW(JR(JQVEPA)) = 0
      IW(JR(JQVENP)) = 0
      IW(JR(JQVECL)) = 0
C
C       KSAME pointer :
C
      IW(JR(JQVESP)) = KVNEW
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
        IW(JR(JQVEBM+IB-1)) = 0
   9  CONTINUE
      IW(JR(JQVELK)) = 0
C
C       vertices :
C
      IW(JR(JQVESC)) = 0
      IW(JR(JQVEOV)) = 0
      IW(JR(JQVEEV)) = 0
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
      RW(JR(JQVEEM)) = -1.
      RW(JR(JQVECF)) = -1.
C
      END
#endif
