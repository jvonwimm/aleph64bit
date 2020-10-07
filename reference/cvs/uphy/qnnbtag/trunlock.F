      SUBROUTINE TRUNLOCK
C------------------------
C     Unlock the tracks
#include "qcde.h"
#include "comnnt.h"
#include "qmacro.h"
C
      DO NFW = IG , ID
      CALL QLUTRK(NFW)
      ENDDO
C
      RETURN
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
C    CALCUL REVIS,YMT [energie vis/qelep,masse transverse,
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
