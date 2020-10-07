      SUBROUTINE SELECT
C--------------------------------
C     Select Z--> qq(bar) events
#include "qcde.h"
#include "comnnt.h"
#include "qmacro.h"
      NEVTYPE = 0
      NBT  = 0
      NEC  = 0
      NECR = 0
      PTOT = 0.
      DO 29 ITK = KFCHT,KLCHT
C---- Selection of good tracks ----
      IF (QP(ITK).GE.0.2.AND.KFRTNT(ITK).GE.4.AND.ABS(QCT(ITK)).LT.0.95
     >.AND.(ABS(QDB(ITK)).LT.2.).AND.ABS(QZB(ITK)).LT.10.) THEN
      NBT = NBT + 1
      PTOT = PTOT + QP(ITK)
      ENDIF
  29  CONTINUE
      IF (NBT.GE.5.AND.PTOT.GE.9.1) THEN
      NEVTYPE = 1
      NZQQ = NZQQ + 1
      ENDIF
C
      RETURN
      END
C
