      SUBROUTINE TAGVAR3
C-----------------------------------------------
      CHARACTER*13 EIGNAM
#include "qcde.h"
#include "comnnt.h"
        DIMENSION EIGVAL(3),PTJ(20),PTLJ(20)
#include "qmacro.h"
C
C A ENLEVER
      NN = 0
      DO 61 I = IG , ID
      IF (XLOCK(I)) GOTO 61
      NN = NN + 1
 61   CONTINUE
C
      IMAJOR = KPDIR(EIGNAM,KRECO)
      ISEMI  = KFOLLO(IMAJOR)
      IMINOR = KFOLLO(ISEMI)
C
C
      SMJETS = 0.
      J = KPDIR(CNAM,KRECO)
      DO 50 I = 1 , NJET
      PTJ(I)  = 0.
      PTLJ(I) = 0.
      IF (I.GT.1)J = KFOLLO(J)
C
      EKKM  = 0.
      PXKKM = 0.
      PYKKM = 0.
      PZKKM = 0.
      DO L = IG , ID
      IF (XSAME(L,J)) THEN
      EKKM = EKKM + QE(L)
      PXKKM= PXKKM+ QX(L)
      PYKKM= PYKKM+ QY(L)
      PZKKM= PZKKM+ QZ(L)
      ENDIF
      ENDDO
C
      XMKKM  = EKKM**2 - PXKKM**2 - PYKKM**2 - PZKKM**2
      IF (XMKKM.LE.0.) THEN
      XMKKM  = 0.
      ELSE
      XMKKM  = SQRT (XMKKM)
      ENDIF
      IF (SCHEME.EQ.'E') THEN
      SMJETS = SMJETS + ABS(QM(J))
      ELSE
      SMJETS = SMJETS + XMKKM
      ENDIF
C
 50   CONTINUE
C
      PTJETS = 0.
      PTLJET = 0.
      KKALL  = 0
C
      JJ = KPDIR(CNAM,KRECO)
      DO 60 I = 1 , NJET
      IF (I.GT.1) JJ = KFOLLO(JJ)
      DO 70 L = IG , ID
      IF (XSAME(L,JJ)) THEN
      PL      = ABS(QPPAR(L,JJ))
      PT      = ABS(QPPER(L,JJ))
      KKALL = KKALL + 1
      PTJ(I)  = PTJ(I)  + PT
      PTLJ(I) = PTLJ(I) + PL * PT
      ENDIF
 70   CONTINUE
 60   CONTINUE
C
      DO 100 I = 1 , NJET
      PTJETS = PTJETS + PTJ(I)
      PTLJET = PTLJET + PTLJ(I)
 100  CONTINUE
C
      PTJETS = PTJETS / PNERC
      PTLJET = PTLJET / PNERC**2
C
       RETURN
       END
C
