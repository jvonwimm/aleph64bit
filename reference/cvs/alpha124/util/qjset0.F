      SUBROUTINE QJSET0 (ICLASS)
CKEY JETS SHAPE /INTERNAL
C----------------------------------------------------------------------
C! Standard set-up for QJ routines : Get numbers
C                                          Author: H.Albrecht 22.2.89
C                                        Modified: E.Blucher  22.6.89
C                                        Modified: E.Blucher  28.9.91
C                                        Modified: E.Blucher  01.2.92
C  Completely rewritten to avoid ASSIGN statements J.Boucrot  24.9.93
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
      LOGICAL LOCKN, LOCK
#include "qcde.h"
#include "qctbuf.h"
      DATA IERR /0/
#include "qmacro.h"
C-----------------------------------------------------------------------
C  Local statement functions :
      NCLASS(I) = IW(KOQVEC+I*KCQVEC+JQVECL)
      NFOLLO(I) = IW(KOQVEC+I*KCQVEC+JQVENP)
      NBMASK(I,N) = IW(KOQVEC+I*KCQVEC+N+JQVEBM-1)
      LOCKN(I,N)=IAND(NBMASK(I,N),KLOCK0(N,KCLARM(NCLASS(I)))).NE.0
      LOCK(I)  = IW(KOQVEC+I*KCQVEC+JQVELK).NE.0.OR.LOCKN(I,1).OR.
     & LOCKN(I,2) .OR. LOCKN(I,3) .OR. LOCKN(I,4) .OR. LOCKN(I,5) .OR.
     & LOCKN(I,6) .OR. LOCKN(I,7) .OR. LOCKN(I,8) .OR. LOCKN(I,9) .OR.
     & LOCKN(I,10).OR.LOCKN(I,11).OR.LOCKN(I,12).OR.LOCKN(I,13).OR.
     & LOCKN(I,14)
C-----------------------------------------------------------------------
      KTBI = 0
      ICL = KSCOLD (ICLASS)
      IF (ICL .EQ. 0)  THEN
        IERR = IERR + 1
        IF (IERR .LT. 10)
     &    CALL QWMESE ('_QJSET0_ Track class does not exist')
        GO TO 900
      ENDIF
C
      IRM = KCLARM(ICL)
      IF (IRM .EQ. 1)  THEN
C
C         reco
C
        IJMP = 3
        GO TO (10,20,30,300,50,60,70,80), KJOPTN(1,1)
   10   N1 = KFCHT
        N2 = KLCHT
        GO TO 200
   20   N1 = KFCOT
        N2 = KLCOT
        GO TO 200
   30   N1 = KFRET
        N2 = KLRET
        GO TO 200
   50   N1 = KFRET
        N2 = KLDCT
        GO TO 200
C---Energy flow with cal objects plus tracks
   60   N1 = KFEFT
        N2 = KLEFT
        GO TO 200
   70   N1 = KFCHT
        N2 = KLCHT
        IF(KNCHT.NE.0.AND.
     &  (KFRIQF(KFCHT).LT.1.OR.KFRIQF(KFCHT).GT.4))CALL QMTERM
     &  ('_QJSET0_ JULIA Version too old to use PCPA')
        GOTO 200
   80   N1 = KFJET
        N2 = KLJET
        GO TO 200
      ELSE IF (IRM .EQ. 2)  THEN
C
C         MC
C
        GO TO (110,120,130,300), KJOPTN(2,1)
  110   N1 = KFMCT
        N2 = KLMCT
        IJMP = 1
        GO TO 200
  120   N1 = KFMCT
        N2 = KLMCT
        IJMP = 2
        GO TO 200
  130   N1 = KFMCT
        N2 = KLMCT
        IJMP = 3
        GO TO 200
      ELSE
        IERR = IERR + 1
        IF (IERR .LT. 10)
     &  CALL QWMESE ('_QJSET0_ Unknown origin (RECO or MC)')
        GO TO 900
      ENDIF
C
 200  CALL QJSET1 (IRM,N1,N2,ICL,IJMP)
C
  300 IF (KJOPTN(IRM,2) .EQ. 0)  GO TO 900
      IFP = IW(KOQPLI+KJOPTN(IRM,2))
      IF (IFP .EQ. 0)  GO TO 900
      ITK = IW(KOQFPA+IFP*KCQFPA+ICL)
C
  310 IF (ITK .EQ. 0)  GO TO 900
      IF (LOCK(ITK))  GO TO 320
      KTBI = KTBI + 1
      IF (KTBI .GT. KTBIMX)  CALL QMTERM
     &    ('_QJSET0_ Maximum number of particles exceeded')
      KTBIT(KTBI) = ITK
  320 ITK = NFOLLO (ITK)
      GO TO 310
C
  900 END
#endif
