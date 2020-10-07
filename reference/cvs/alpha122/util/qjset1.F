      SUBROUTINE QJSET1 (IRM,N1,N2,ICL,IJMP)
CKEY JETS SHAPE /INTERNAL
C----------------------------------------------------------------------
C! Auxiliary to QJSET0
C                                          Author: J.Boucrot  24.9.93
C----------------------------------------------------------------------
#ifndef DOC
      LOGICAL LOCKN, LOCK
#include "qcde.h"
#include "qctbuf.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
C  Local statement functions :
      NCLASS(I) = IW(KOQVEC+I*KCQVEC+JQVECL)
      NFOLLO(I) = IW(KOQVEC+I*KCQVEC+JQVENP)
      NSAME(I) = IW(KOQVEC+I*KCQVEC+JQVESP)
      NSTABC(I) = IW(KOQVEC+I*KCQVEC+JQVESC)
      NBMASK(I,N) = IW(KOQVEC+I*KCQVEC+N+JQVEBM-1)
      LOCKN(I,N)=IAND(NBMASK(I,N),KLOCK0(N,KCLARM(NCLASS(I)))).NE.0
      LOCK(I)  = IW(KOQVEC+I*KCQVEC+JQVELK).NE.0.OR.LOCKN(I,1).OR.
     & LOCKN(I,2) .OR. LOCKN(I,3) .OR. LOCKN(I,4) .OR. LOCKN(I,5) .OR.
     & LOCKN(I,6) .OR. LOCKN(I,7) .OR. LOCKN(I,8) .OR. LOCKN(I,9) .OR.
     & LOCKN(I,10).OR.LOCKN(I,11).OR.LOCKN(I,12).OR.LOCKN(I,13).OR.
     & LOCKN(I,14)
C-----------------------------------------------------------------------
      IF (IJMP.LT.1.OR.IJMP.GT.3) GO TO 999
      DO 260 NTK = N1,N2
        IF (NTK .GT. KLAST .AND. NTK .LT. KFEHT)  GO TO 260
        IF(IRM.EQ.1.AND.KJOPTN(1,1).EQ.7.AND.(KFRIQF(NTK).NE.1.AND.
     &   KFRIQF(NTK).NE.3)) GO TO 260
        ITK = NTK
  210   IF (NCLASS(ITK) .NE. ICL .OR. LOCK(ITK))  GO TO 250
        GO TO (220,230,240) IJMP
C
  220   LS = NSTABC(ITK)
        IF (LS .EQ. 1 .OR. LS .EQ. 2 .OR. LS .EQ. -3)  GO TO 240
        GO TO 250
  230   IF (NSTABC(ITK) .LE. 0)  GO TO 250
C
  240   KTBI = KTBI + 1
        IF (KTBI .GT. KTBIMX)  CALL QMTERM
     &    ('_QJSET1_ Maximum number of particles exceeded')
        KTBIT(KTBI) = ITK
        GO TO 260
C
  250   ITK = NSAME(ITK)
        IF (ITK .NE. NTK)  GO TO 210
  260 CONTINUE
C
  999 RETURN
      END
#endif
