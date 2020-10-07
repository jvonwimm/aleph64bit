      SUBROUTINE QFNEOB
CKEY FILL PCPA EFLOW /INTERNAL
C----------------------------------------------------------------------
C!  - Fill NEOB section from PCQA bank.
C!
C!   Author   :- E. Blucher     29-NOV-1990
C! Modified   :- J. Carr        10-May-1991
C! Modified   :- E. Blucher     02-Feb-1992
C!     kfnet = first neutral track
C!     klnet = last neutral track
C!     knnet = number of neutral tracks
C!     Objects from PCQA stored as 'NEOB'
C
C  Called from QFILL
C!======================================================================
#ifndef DOC
      SAVE INIT,IERR,INEOB,NNEOB
#include "qcde.h"
      DATA INIT /0/, IERR /0/
#include "qmacro.h"
C----------------------------------------------------------------------
C
      JR(I) = KOQVEC + ITK * KCQVEC + I
      JQDET(I,N) = KOQDET + (KFFRD+I-1) * KCQDET + N
C----------------------------------------------------------------------
C
      IF (INIT .EQ. 0) THEN
        INIT = 1
        INEOB = KPCOMP ('NEOB')
        NNEOB = KFPADR(INEOB)
      ENDIF
C
      JPCQA = IW(NAPCQA)
      IF(JPCQA.EQ.0)GOTO 999
C
      KNNET = IW(JPCQA+LMHLEN)
      KFNET = KFFRT
      KLNET=KFNET + KNNET -1
C
      IF (KLNET .GE. KLFRT)  CALL QSBANK ('QVEC', KLNET+200)
      IF (KFFRD + KNNET -1 .GE. IW(KOQDET))
     &   CALL QSBANK ('QDET', KFFRD + KNNET -1 + 100)
C
      DO 10 I=1,KNNET
        ITK=KFNET+I-1
        PX = RTABL(JPCQA,I,JPCQPX)
        PY = RTABL(JPCQA,I,JPCQPY)
        PZ = RTABL(JPCQA,I,JPCQPZ)
        EN = RTABL(JPCQA,I,JPCQEN)
        RW(JR(JQVEQX)) = PX
        RW(JR(JQVEQY)) = PY
        RW(JR(JQVEQZ)) = PZ
        RW(JR(JQVEQE)) = EN
        PN = SQRT( PX**2+PY**2+PZ**2 )
        RW(JR(JQVEQP)) = PN
        AM=QSQT(EN**2-PN**2)
        RW(JR(JQVEQM)) = AM
        RW(JR(JQVECH)) = 0.
        IW(JR(JQVETN)) = I
        IW(JR(JQVEPA)) = INEOB
        IW(JR(JQVENP)) = IW(KOQFPA+NNEOB*KCQFPA+1)
        IW(KOQFPA+NNEOB*KCQFPA+1) = ITK
        IW(JR(JQVESC)) = 1
        IW(JR(JQVEKS)) = 0
        IW(JR(JQVECL)) = 1
        IW(JR(JQVEQD)) = JQDET(I,0)
        IW(JR(JQVESP)) = ITK
        IW(JR(JQVEOV)) = 0
        IW(JR(JQVEEV)) = 0
        IW(JR(JQVEND)) = 0
        IW(JR(JQVENO)) = 0
        IW(JR(JQVENM)) = 0
          DO 9 IDUM=1,KLOCKM
            IW(JR(JQVEBM+IDUM-1)) = 0
9         CONTINUE
        IW(JR(JQVELK)) = 0
        RW(JR(JQVEEW)) = 0.
        RW(JR(JQVEDB)) = 0.
        RW(JR(JQVEZB)) = 0.
        RW(JR(JQVESD)) = 0.
        RW(JR(JQVESZ)) = 0.
        RW(JR(JQVEEM)) = -1.
        RW(JR(JQVECF)) = -1.
C---Fill QDET
          IW(JQDET(I,JQDEAF)) = KQZER
          IW(JQDET(I,JQDEAL)) = KQZER
          IW(JQDET(I,JQDENT)) = 0
          IW(JQDET(I,JQDEAT)) = KQZER
          IW(JQDET(I,JQDEAE)) = KQZER
          IW(JQDET(I,JQDEAH)) = KQZER
          IW(JQDET(I,JQDEAM)) = KQZER
          IW(JQDET(I,JQDECF)) = 0
          IW(JQDET(I,JQDEEC)) = KQZER
          IW(JQDET(I,JQDEHC)) = KQZER
          IW(JQDET(I,JQDEET)) = KQZER
          IW(JQDET(I,JQDEFI)) = KQZER
          IW(JQDET(I,JQDENF)) = 0
          IW(JQDET(I,JQDENE)) = 0
          IW(JQDET(I,JQDENH)) = 0
          IW(JQDET(I,JQDEEF)) = KQZER
          IW(JQDET(I,JQDEPC)) = JPCQA + LMHLEN + (I-1)*LCOLS(JPCQA)
          IW(JQDET(I,JQDEEG)) = KQZER
          IW(JQDET(I,JQDEMU)) = KQZER
          IW(JQDET(I,JQDEPG)) = KQZER
          IW(JQDET(I,JQDEPG)) = KQZER
C--- set bit mask
        CALL QSBITM(ITK)
   10 CONTINUE
C   UPDATE KLOCUS POINTERS.
      DO 20 N = KSNET+1,KSMCT
        KLOCUS(1,N) = KLOCUS(2,KSNET) + 1
   20 KLOCUS(2,N) = KLOCUS(2,KSNET)
      KFFRT = KLOCUS(2,KSNET) + 1
      KFFRD=KFFRD+KNNET
  999 RETURN
      END
#endif
