      SUBROUTINE TRLOCK
C----------------------------------------------------------
C     Lock bad tracks not used to reconstruct the variables
#include "qcde.h"
#include "comnnt.h"
#include "qmacro.h"
C
      DO  13 NFW = IG , ID
      IF (QP(NFW).LE.0.2)                               CALL QLTRK(NFW)
      IF (QP(NFW).GE.100.)                              CALL QLTRK(NFW)
      IF (ABS(QZ(NFW))/QP(NFW).GT.0.951)                CALL QLTRK(NFW)
      IF (QCH(NFW).EQ.0.0) GOTO 13
      IF (KFRTNT(NFW).LE.4)                             CALL QLTRK(NFW)
      IF (ABS(QDB(NFW)).GE.2.0.OR.ABS(QZB(NFW)).GE.10.) CALL QLTRK(NFW)
      PT = SQRT (QX(NFW)**2 + QY(NFW)**2)
      IF (PT.LE.0.25)                                   CALL QLTRK(NFW)
  13  CONTINUE
C
      RETURN
      END
C
