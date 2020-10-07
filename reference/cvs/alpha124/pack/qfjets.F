      SUBROUTINE QFJETS
CKEY FILL JETS EFLOW /INTERNAL
C----------------------------------------------------------------------
C!  - Fill EJET section from EJET bank.
C!
C!   Author   :- E. Blucher     14-JUL-1992
C!     KFJET = first jet
C!     KLJET = last jet
C!     KNJET = number of jets
C!     Objects from EJET stored as 'EJET'
C!
C!  Called from QFILL
C!======================================================================
#ifndef DOC
      SAVE INIT,IEJET,NEJET
#include "qcde.h"
#include "ejetjj.h"
      DATA INIT /0/
#include "qmacro.h"
C----------------------------------------------------------------------
C
      JR(I) = KOQVEC + ITK * KCQVEC + I
      JQDET(I,N) = KOQDET + (KFFRD+I-1) * KCQDET + N
      JE(I) = KOQVEC + IE * KCQVEC + I
C----------------------------------------------------------------------
C
      IF (INIT .EQ. 0) THEN
        INIT = 1
        IEJET = KPCOMP ('EJET')
        NEJET = KFPADR(IEJET)
      ENDIF
C
      IF(KEFOPT.EQ.2)THEN
         JEJET = NLINK('EJET',0)
       ELSE
         JEJET = NLINK('EJET',3)
       ENDIF
      IF(JEJET.EQ.0)GOTO 999
C
      KNJET = IW(JEJET+LMHLEN)
      KFJET = KFFRT
      KLJET=KFJET + KNJET -1
C
      IF (KLJET .GE. KLFRT)  CALL QSBANK ('QVEC', KLJET+200)
C
      DO 10 I=1,KNJET
        ITK=KFJET+I-1
        PX = RTABL(JEJET,I,JEJEPX)
        PY = RTABL(JEJET,I,JEJEPY)
        PZ = RTABL(JEJET,I,JEJEPZ)
        EN = RTABL(JEJET,I,JEJEPE)
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
        IW(JR(JQVEPA)) = IEJET
        IW(JR(JQVENP)) = IW(KOQFPA+NEJET*KCQFPA+1)
        IW(KOQFPA+NEJET*KCQFPA+1) = ITK
        IW(JR(JQVESC)) = 1
        IW(JR(JQVEKS)) = 0
        IW(JR(JQVECL)) = 1
        IW(JR(JQVEQD)) = KOQDET
        IW(JR(JQVESP)) = ITK
        IW(JR(JQVEOV)) = 0
        IW(JR(JQVEEV)) = 0
        IW(JR(JQVEND)) = 0
        IW(JR(JQVENO)) = 0
        IW(JR(JQVENM)) = 0
C---Set LOCK bits.
C
C---ZERO LOCK BITS
C
          DO 9 IDUM=1,KLOCKM
            IW(JR(JQVEBM+IDUM-1)) = 0
9         CONTINUE
C
C---LOOP OVER EFOL TRACKS TO FIND OBJECTS BELONGING TO JETS
C
         DO 7 IE=KFEFT,KLEFT
            IF(KEFOLJ(IE).EQ.I)THEN
              DO 8 IDUM=1,KLOCKM
                IW(JR(JQVEBM+IDUM-1)) =
     &          IOR(IW(JR(JQVEBM+IDUM-1)),IW(JE(JQVEBM+IDUM-1)))
 8         CONTINUE
         ENDIF
7       CONTINUE

        IW(JR(JQVELK)) = 0
        RW(JR(JQVEEW)) = 0.
        RW(JR(JQVEDB)) = 0.
        RW(JR(JQVEZB)) = 0.
        RW(JR(JQVESD)) = 0.
        RW(JR(JQVESZ)) = 0.
        RW(JR(JQVEEM)) = -1.
        RW(JR(JQVECF)) = -1.
   10 CONTINUE
C   UPDATE KLOCUS POINTERS.
      DO 20 N = KSJET+1,KSMCT
        KLOCUS(1,N) = KLOCUS(2,KSJET) + 1
   20 KLOCUS(2,N) = KLOCUS(2,KSJET)
      KFFRT = KLOCUS(2,KSJET) + 1
  999 RETURN
      END
#endif
