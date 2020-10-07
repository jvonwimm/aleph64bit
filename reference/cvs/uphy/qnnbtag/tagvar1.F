      SUBROUTINE TAGVAR1
C--------------------------------------------------------------------
      CHARACTER*13 EIGNAM
#include "qcde.h"
#include "comnnt.h"
      DIMENSION EIGVAL(3)
#include "qmacro.h"
C
      CALL QJEIG(EIGVAL,'EIGAXE',KRECO)
      EIGNAM = 'EIGAXE'
      I1 = KPDIR(EIGNAM,KRECO)
      I2 = KFOLLO(I1)
      I3 = KFOLLO(I2)
C
      REVIS    = ENERC / QELEP
C
      PROD = 0.
      DO 1 ITK = IG , ID
      IF (XLOCK(ITK)) GOTO 1
      CALL PR_SCAL(2,QX(ITK),QY(ITK),QZ(ITK),QX(I3),QY(I3),QZ(I3),U)
  1   PROD = PROD + ABS(U)
C
C COR 2/3/94
      IF(I3.LE.0) RETURN
      IF(QP(I3).LE.0.0001) RETURN
      PROD = PROD / QP(I3)
      YMT  = QELEP * PROD / ENERC
      RETURN
      END
C
C    Properties of the most energetic Jet of the Hemisphere
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
