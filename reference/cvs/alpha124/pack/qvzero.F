      SUBROUTINE QVZERO (ITK)
CKEY VECTOR TRACKS ZERO /USER
C----------------------------------------------------------------------
C! Zero track ITK.
C                                                   H.Albrecht   2.1.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      DATA IERR/0/
      JR(J) = KOQVEC + ITK * KCQVEC + J
C----------------------------------------------------------------------
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
      IF (ITK .LE. KLUST)  GO TO 90
C
C       particle code, class :
C
      IW(JR(JQVEPA)) = 0
      IW(JR(JQVENP)) = 0
      IW(JR(JQVECL)) = 0
C
C       KSAME pointer :
C
      ITK0 = IW(JR(JQVESP))
      ITK1 = ITK0
   20 ITK2 = IW(KOQVEC+ITK1*KCQVEC+JQVESP)
      IF (ITK2 .NE. ITK)  THEN
        ITK1 = ITK2
        GO TO 20
      ENDIF
      IW(KOQVEC+ITK1*KCQVEC+JQVESP) = ITK0
      IW(JR(JQVESP)) = ITK
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
  9   CONTINUE
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
      RW(JR(JQVEEM)) = 0.
      RW(JR(JQVEEM+1)) = 0.
      RW(JR(JQVEEM+2)) = 0.
      RW(JR(JQVEEM+3)) = 0.
      RW(JR(JQVEEM+4)) = 0.
      RW(JR(JQVEEM+5)) = 0.
      RW(JR(JQVEEM+6)) = 0.
      RW(JR(JQVEEM+7)) = 0.
      RW(JR(JQVEEM+8)) = 0.
      RW(JR(JQVEEM+9)) = 0.
      RW(JR(JQVECF)) = -1.
C
      IF (ITK .LE. KLMCT .AND. ITK .GE. KFRET)  THEN
        IERR = IERR + 1
        IF (IERR .LE. 10)  CALL QWMESE
     &  ('_QVZERO_ Warning: Unviolable particle has been modified')
      ENDIF
C
   90 END
#endif
