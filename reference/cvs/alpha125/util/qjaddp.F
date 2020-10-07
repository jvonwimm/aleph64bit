      SUBROUTINE QJADDP (PSUM,CNAM,ICLASS)
CKEY SHAPE JETS /USER
C----------------------------------------------------------------------
C! Add 4-vectors
C                                                    H.Albrecht 22.2.89
C----------------------------------------------------------------------
#ifndef DOC
      CHARACTER *(*) CNAM
#include "qcdesh.h"
#include "qctbuf.h"
C
      CALL QJSET0 (ICLASS)
C
      QTBOX(1) = 0.
      QTBOY(1) = 0.
      QTBOZ(1) = 0.
      QTBOE(1) = 0.
C
      DO 10 I=1,KTBI
        QTBOX(1) = QTBOX(1) + RW(KOQVEC+KTBIT(I)*KCQVEC+JQVEQX)
        QTBOY(1) = QTBOY(1) + RW(KOQVEC+KTBIT(I)*KCQVEC+JQVEQY)
        QTBOZ(1) = QTBOZ(1) + RW(KOQVEC+KTBIT(I)*KCQVEC+JQVEQZ)
        QTBOE(1) = QTBOE(1) + RW(KOQVEC+KTBIT(I)*KCQVEC+JQVEQE)
        KTBOF(KTBI) = 1
   10 CONTINUE
C
      PSUM = SQRT (QTBOX(1)**2 + QTBOY(1)**2 + QTBOZ(1)**2)
      IF (CNAM .NE. ' ')  CALL QJSAVE (CNAM,1,ICLASS,0,0,0)
C
      END
#endif
