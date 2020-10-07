      SUBROUTINE QJMISS (PSUM,CNAM,ICLASS,ITKTOT)
CKEY SHAPE /USER
C----------------------------------------------------------------------
C! Missing E,p,m
C                                                    H.Albrecht 22.2.89
C----------------------------------------------------------------------
#ifndef DOC
      CHARACTER *(*) CNAM
#include "qcde.h"
#include "qctbuf.h"
      J1(I) = KOQVEC + ITKTOT * KCQVEC + I
C-----------------------------------------------------------------------
      CALL QJSET0 (ICLASS)
C
      QTBOX(1) = 0.
      QTBOY(1) = 0.
      QTBOZ(1) = 0.
      QTBOE(1) = 0.
C
C
      DO 10 I=1,KTBI
        QTBOX(1) = QTBOX(1) + RW(KOQVEC+KTBIT(I)*KCQVEC+JQVEQX)
        QTBOY(1) = QTBOY(1) + RW(KOQVEC+KTBIT(I)*KCQVEC+JQVEQY)
        QTBOZ(1) = QTBOZ(1) + RW(KOQVEC+KTBIT(I)*KCQVEC+JQVEQZ)
        QTBOE(1) = QTBOE(1) + RW(KOQVEC+KTBIT(I)*KCQVEC+JQVEQE)
        KTBOF(KTBI) = 1
   10 CONTINUE
C
      IF (ITKTOT .EQ. 0)  THEN
        QTBOX(1) = - QTBOX(1)
        QTBOY(1) = - QTBOY(1)
        QTBOZ(1) = - QTBOZ(1)
        PSUM = SQRT (QTBOX(1)**2 + QTBOY(1)**2 + QTBOZ(1)**2)
        QTBOE(1) = AMAX1 (QELEP - QTBOE(1), PSUM)
      ELSE
        QTBOX(1) = RW(J1(JQVEQX)) - QTBOX(1)
        QTBOY(1) = RW(J1(JQVEQY)) - QTBOY(1)
        QTBOZ(1) = RW(J1(JQVEQZ)) - QTBOZ(1)
        PSUM = SQRT (QTBOX(1)**2 + QTBOY(1)**2 + QTBOZ(1)**2)
        QTBOE(1) = AMAX1 (RW(J1(JQVEQE)) - QTBOE(1), PSUM)
      ENDIF
      IF (CNAM .NE. ' ')  CALL QJSAVE (CNAM,1,ICLASS,0,0,0)
C
      END
#endif
