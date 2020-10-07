      SUBROUTINE QMTHRU
CKEY SHAPE /INTERNAL
C----------------------------------------------------------------------
C! Get pre-calculated Thrust from bank 'PTHR'
C                                                    J.Boucrot 18.10.94
C  Called from QJTHRU
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "pthrjj.h"
#include "qctbuf.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
      JPTHR=IW(NAPTHR)
      IF (JPTHR.GT.0)  THEN
         KTBO=1
         QTBOR(1)=RTABL(JPTHR,1,JPTHTH)
         QTBOX(1)=RTABL(JPTHR,1,JPTHPX)
         QTBOY(1)=RTABL(JPTHR,1,JPTHPY)
         QTBOZ(1)=RTABL(JPTHR,1,JPTHPZ)
         QTBOE(1)=RTABL(JPTHR,1,JPTHPE)
      ELSE
         KTBO=0
         QTBOR(1)=0.
         QTBOX(1)=0.
         QTBOY(1)=0.
         QTBOZ(1)=0.
         QTBOE(1)=0.
      ENDIF
      RETURN
      END
#endif
