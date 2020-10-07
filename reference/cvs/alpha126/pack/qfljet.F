      SUBROUTINE QFLJET
CKEY FILL JETS LEPTONS /INTERNAL
C----------------------------------------------------------------------
C!  - Fill LJET section from PLJT bank ( jets built by QSELEP )
C!
C!   Author   :- J. Boucrot     10-Nov-1994
C!     KLFJET = first jet
C!     KLLJET = last jet
C!     KLNJET = number of jets
C!     Objects from PLJT stored as 'LJET'
C!
C!  Called from QFLEPT
C!======================================================================
#ifndef DOC
      SAVE INIT,ILJET,NLJET
#include "qcde.h"
#include "pljtjj.h"
      DATA INIT /0/
#include "qmacro.h"
C----------------------------------------------------------------------
C inline function :
      JR(I) = KOQVEC + ITK * KCQVEC + I
C----------------------------------------------------------------------
C
      IF (INIT .EQ. 0) THEN
        INIT = 1
        ILJET = KPCOMP ('LJET')
        NLJET = KFPADR(ILJET)
      ENDIF
C
      NAPLJT = NAMIND ('PLJT')
      JLJET = IW(NAPLJT)
      KLNJET = 0
      IF (JLJET.EQ.0) GO TO 999
C
      KLNJET = IW(JLJET+LMHLEN)
      KLFJET = KFFRT
      KLLJET = KLFJET + KLNJET -1
C
      IF (KLLJET .GE. KLFRT)  CALL QSBANK ('QVEC', KLLJET+200)
C
      DO 10 I=1,KLNJET
        ITK=KLFJET+I-1
        PX = RTABL(JLJET,I,JPLJPX)
        PY = RTABL(JLJET,I,JPLJPY)
        PZ = RTABL(JLJET,I,JPLJPZ)
        EN = RTABL(JLJET,I,JPLJPE)
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
        IW(JR(JQVEPA)) = ILJET
        IW(JR(JQVENP)) = IW(KOQFPA+NLJET*KCQFPA+1)
        IW(KOQFPA+NLJET*KCQFPA+1) = ITK
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
        DO 9 IDUM=1,KLOCKM
 9      IW(JR(JQVEBM+IDUM-1)) = 0
        IW(JR(JQVELK)) = 0
        RW(JR(JQVEEW)) = 0.
        RW(JR(JQVEDB)) = 0.
        RW(JR(JQVEZB)) = 0.
        RW(JR(JQVESD)) = 0.
        RW(JR(JQVESZ)) = 0.
        RW(JR(JQVEEM)) = -1.
        RW(JR(JQVECF)) = -1.
 10   CONTINUE
      KFFRT = KFFRT+KLNJET
  999 RETURN
      END
#endif
