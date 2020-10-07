      SUBROUTINE QJHEMI (CSAM,COPP,ICLASS,ITKAXI,CUTCOS)
CKEY SHAPE /USER
C----------------------------------------------------------------------
C! Divide the event into 2 hemispheres
C                                                    H.Albrecht 22.2.89
C----------------------------------------------------------------------
#ifndef DOC
      CHARACTER *(*) CSAM,COPP
#include "qcde.h"
#include "qctbuf.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
      CALL QJSET0 (ICLASS)
C
      QTBOX(1) = 0.
      QTBOY(1) = 0.
      QTBOZ(1) = 0.
      QTBOE(1) = 0.
      QTBOX(2) = 0.
      QTBOY(2) = 0.
      QTBOZ(2) = 0.
      QTBOE(2) = 0.
C
      PX = QX(ITKAXI)
      PY = QY(ITKAXI)
      PZ = QZ(ITKAXI)
      P  = QP(ITKAXI)
C
      DO 10 I=1,KTBI
        IF (PX * QX(KTBIT(I)) + PY * QY(KTBIT(I)) +
     &      PZ * QZ(KTBIT(I)) .GT. CUTCOS * P * QP(KTBIT(I)))  THEN
          QTBOX(1) = QTBOX(1) + QX(KTBIT(I))
          QTBOY(1) = QTBOY(1) + QY(KTBIT(I))
          QTBOZ(1) = QTBOZ(1) + QZ(KTBIT(I))
          QTBOE(1) = QTBOE(1) + QE(KTBIT(I))
          KTBOF(I) = 1
        ELSE
          QTBOX(2) = QTBOX(2) + QX(KTBIT(I))
          QTBOY(2) = QTBOY(2) + QY(KTBIT(I))
          QTBOZ(2) = QTBOZ(2) + QZ(KTBIT(I))
          QTBOE(2) = QTBOE(2) + QE(KTBIT(I))
          KTBOF(I) = 2
        ENDIF
   10 CONTINUE
C
      IF (CSAM .NE. ' ')  CALL QJSAVE (CSAM,1,ICLASS,0,0,1)
      IF (COPP .NE. ' ')  CALL QJSAVE (COPP,2,ICLASS,0,0,1)
C
      END
#endif
