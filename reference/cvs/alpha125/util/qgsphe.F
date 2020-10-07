      SUBROUTINE QGSPHE
CKEY SHAPE /INTERNAL
C----------------------------------------------------------------------
C   Author   : E. Blucher     27-FEB-1989
C
C   Libraries required: CERNLIB (VSCALE,EIGRS1,SORTZV)
C
C   Description
C   ===========
C!  Calculate sphericity
C   from momentum vectors stored in QCTBUF.INC.
C
C   QTBOX,Y,Z(1) = major axis (sphericity axis)
C   QTBOX,Y,Z(2) = semi-major axis
C   QTBOX,Y,Z(3) = minor axis
C   QTBOR(1,2,3) = eigenvalues in decending order
C         sphericity = 1.5*(1-qtbor(1))
C         aplanarity = 1.5*qtbor(3)
C         planarity = qtbor(3)/qtbor(2)
C======================================================================
#ifndef DOC
#include "qctbuf.h"
      DIMENSION P(3),EVAL(3),EVEC(3,3),WK(14),TT(3,3),IND(3)
C----------------------------------------------------------------------
      IF(KTBI.LT.3)THEN
        IF(KTBI.EQ.2)GOTO 1000
        IF (KTBI .EQ. 1)  THEN
          PINOM=SQRT(QTBIX(1)**2 + QTBIY(1)**2 + QTBIZ(1)**2)
          INOM = 1
          GO TO 1010
        ENDIF
        QTBOX(1)=0.
        QTBOY(1)=0.
        QTBOZ(1)=0.
        QTBOE(1)=0.
        GO TO 1020
      ENDIF
C
C---Compute momentum tensor.
C
      DO 10 I=1,3
        DO 9 J=1,3
          TT(I,J)=0.
    9   CONTINUE
   10 CONTINUE
      DO 501 J=1,KTBI
        P(1)=QTBIX(J)
        P(2)=QTBIY(J)
        P(3)=QTBIZ(J)
        DO 502 I=1,3
          DO 503 K=1,I
              TT(I,K)=TT(I,K)+P(I)*P(K)
  503     CONTINUE
  502   CONTINUE
  501 CONTINUE
      TT(1,2)=TT(2,1)
      TT(1,3)=TT(3,1)
      TT(2,3)=TT(3,2)
C
      CALL VSCALE(TT,1./(TT(1,1)+TT(2,2)+TT(3,3)),TT,9)
      CALL EISRS1(3,3,TT,EVAL,EVEC,IER,WK)
      CALL SORTZV(EVAL,IND,3,1,1,0)
C
      IF(IER.NE.0)THEN
        GO TO 9999
      ENDIF
C--- Store major axis (sphericity axis), semi-major axis, minor axis,
C--- and eigenvalues in decending order.
      DO 20 I=1,3
        QTBOX(I)=EVEC(1,IND(I))
        QTBOY(I)=EVEC(2,IND(I))
        QTBOZ(I)=EVEC(3,IND(I))
        QTBOE(I)=1.
        QTBOR(I)=EVAL(IND(I))
   20 CONTINUE
      GO TO 9999
C
C---SPECIAL CASE -- ONLY TWO TRACKS
 1000 P1=SQRT(QTBIX(1)**2 + QTBIY(1)**2 + QTBIZ(1)**2)
      P2=SQRT(QTBIX(2)**2 + QTBIY(2)**2 + QTBIZ(2)**2)
      INOM=1
      PINOM=P1
      IF(P2.GE.P1)THEN
        INOM=2
        PINOM=P2
      ENDIF
 1010 QTBOX(1)=QTBIX(INOM)/PINOM
      QTBOY(1)=QTBIY(INOM)/PINOM
      QTBOZ(1)=QTBIZ(INOM)/PINOM
      QTBOE(1)=1.
 1020 QTBOR(1)=1.
      DO 504 I=2,3
        QTBOX(I)=0.
        QTBOY(I)=0.
        QTBOZ(I)=0.
        QTBOR(I)=0.
        QTBOE(I)=0.
  504 CONTINUE
 9999 CONTINUE
      END
#endif
