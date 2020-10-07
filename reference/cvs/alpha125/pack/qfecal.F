      SUBROUTINE QFECAL (NPECO, EN, TH, PH, SIGEN, SIGTH, SIGPH)
CKEY FILL CAL /INTERNAL
C----------------------------------------------------------------------
C! Get ENergy, THeta, PHi from row NPECO in ECAL bank PECO.
C! SIGEN etc : error**2
C
C!Called from QFCOBJ
C!                                                  H.Albrecht 03.03.89
C!                                                  E.Blucher  04.04.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE Z, INIT
#include "qcde.h"
      DIMENSION ENF(2),THF(2),PHF(2)
      DIMENSION Z(4)
      DATA Z /1.,1.,1.,.17/, INIT /0/
#include "bmacro.h"
C----------------------------------------------------------------------
C          get constants/cuts from bank CEEW
      IF (INIT .EQ. 0)  THEN
        INIT = 1
        CALL QMBANK ('CEEW',
     &    '_QFECAL_ ECAL energy weight factors and errors', Z, 4)
      ENDIF
C
      JPECO = IW(NAPECO)
      JPEC1 = NLINK('PECO',1)
      IF (JPEC1.GT.0) JPECO = JPEC1
C
C
      TH = RTABL(JPECO,NPECO,JPECTH)
      PH = RTABL(JPECO,NPECO,JPECPH)
      F1 = RTABL(JPECO,NPECO,JPECE1)
      F2 = RTABL(JPECO,NPECO,JPECE2)
C            recalibrate energy
      EN = RTABL(JPECO,NPECO,JPECEC) *
     &     (F1 * Z(1) + F2 * Z(2) + (1. - F1 - F2) * Z(3))
      IF(EN.LT.1.E-9)EN=1.E-9
C
      SIGEN = Z(4)**2 * EN
      SIGTH = 0.
      SIGPH = 0.
C
      END
#endif
