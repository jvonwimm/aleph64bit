      SUBROUTINE QJSETU (ICLASS)
CKEY JETS SHAPE /USER
C----------------------------------------------------------------------
C! Standard set-up for QJ routines
C                                                    H.Albrecht 22.2.89
C                                 Modified:          E.Blucher  28.9.91
C                                 Modified:          E.Blucher  30.1.92
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
#include "qctbuf.h"
C-----------------------------------------------------------------------
      CALL QJSET0 (ICLASS)
C
      DO 20 I=1, KTBI
        QTBIX(I) = RW(KOQVEC+KTBIT(I)*KCQVEC+JQVEQX)
        QTBIY(I) = RW(KOQVEC+KTBIT(I)*KCQVEC+JQVEQY)
        QTBIZ(I) = RW(KOQVEC+KTBIT(I)*KCQVEC+JQVEQZ)
   20   QTBIE(I) = RW(KOQVEC+KTBIT(I)*KCQVEC+JQVEQE)
C
  90  END
#endif
