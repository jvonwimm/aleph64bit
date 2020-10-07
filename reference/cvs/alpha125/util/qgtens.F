      SUBROUTINE QGTENS
CKEY SHAPE /INTERNAL
C-----------------------------------------------------
C     Author : M.N.Minard , M.Pepe
C        Modified version of QGSPHE, E. Blucher.
C     Libraries required CERNLIB ( VSCALE,EIGRS1 )
C
C!     Calculate normalised momentum tensor
C     eigen-vectors and eigen-values from vectors in QCTBUF
C
C     QTBOX , Y , Z (1) = major axis
C     QTBOX , Y , Z (2) = semi-major axis
C     QTBOX , Y , Z (3) = minor axis
C     QTBOR(1,2,3)      = eigenvalues in descending order.
C---------------------------------------------------------------
#ifndef DOC
#include "qctbuf.h"
      DIMENSION TT(3,3),PVEC(3),EIGVAL(3),EIGVEC(3,3),WK(14),IR(3)
C----------------------------------------------------------------------
      IF (KTBI.LT.3) THEN
         IF (KTBI.EQ.2) GO TO 1000
         IF (KTBI.EQ.1) THEN
            INOM = 1
            PINOM = QTBIE(1)
            GO TO 1010
         ENDIF
         QTBOX(1) = 0.
         QTBOY(1) = 0.
         QTBOZ(1) = 0.
         QTBOE(1) = 0.
         GO TO 1020
      ENDIF
C
      SUMEI = 0.
      DO 10 I =1,3
      DO 10 J =1,3
      TT (I,J) = 0.
 10   CONTINUE
      DO 500 J = 1,KTBI
      PVEC (1) = QTBIX(J)
      PVEC (2) = QTBIY(J)
      PVEC (3) = QTBIZ(J)
      SUMEI = SUMEI + QTBIE(J)
      DO 480 I = 1,3
      DO 470 K = 1,I
      TT (I,K) = TT(I,K)+PVEC(I)*PVEC(K)/QTBIE(J)
 470  CONTINUE
 480  CONTINUE
 500  CONTINUE
      TT (1,2) = TT(2,1)
      TT (1,3) = TT(3,1)
      TT (2,3) = TT(3,2)
C
C-    Normalise vector
C
      CALL VSCALE(TT(1,1),1./SUMEI,TT(1,1),9)
      CALL EISRS1(3,3,TT,EIGVAL,EIGVEC,IERR,WK)
      CALL SORTZV(EIGVAL,IR,3,1,1,0)
      IF (IERR.NE.0) GO TO 9999
C
C-    Store axis
C
      DO 520 I =1,3
      QTBOX(I) = EIGVEC(1,IR(I))
      QTBOY(I) = EIGVEC(2,IR(I))
      QTBOZ(I) = EIGVEC(3,IR(I))
      QTBOE(I)=1.
      QTBOR(I) = EIGVAL(IR(I))
 520  CONTINUE
      GO TO 9999
C
C-    2 tracks only
C
 1000 P1 = SQRT (QTBIX(1)**2 + QTBIY(1)**2 + QTBIZ(1)**2)
      P2 = SQRT (QTBIX(2)**2 + QTBIY(2)**2 + QTBIZ(2)**2)
      INOM = 1
      PINOM = P1
      IF (P2.GT.P1) THEN
         INOM = 2
         PINOM = P2
      ENDIF
 1010 QTBOX(1) = QTBIX(INOM)/PINOM
      QTBOY(1) = QTBIY(INOM)/PINOM
      QTBOZ(1) = QTBIZ(INOM)/PINOM
      QTBOE(1)=1.
 1020 QTBOR(1) = 1.
      DO 600 I = 2,3
      QTBOX(I) = 0.
      QTBOY(I) = 0.
      QTBOZ(I) = 0.
      QTBOR(I) = 0.
      QTBOE(I)=0.
 600  CONTINUE
 9999 CONTINUE
      RETURN
      END
#endif
