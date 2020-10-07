      SUBROUTINE QFHCAL (NPHCO, EN, TH, PH, SIGEN, SIGTH, SIGPH)
CKEY FILL CAL /INTERNAL
C----------------------------------------------------------------------
C! Get ENergy, THeta, PHi from row NPHCO in ECAL bank PHCO.
C! SIGEN etc : error**2
C!Called from QFCOBJ
C!                                                  H.Albrecht 03.03.89
C!                                                  E.Blucher  04.04.89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE Z,INIT
#include "qcde.h"
      DIMENSION ENF(2),THF(2),PHF(2)
      DIMENSION Z(2)
      DATA Z /1.,.8/, INIT /0/
#include "bmacro.h"
C----------------------------------------------------------------------
C
C          get constants/cuts from bank CHEW
C
      IF (INIT .EQ. 0)  THEN
        INIT = 1
        CALL QMBANK ('CHEW',
     &    '_QFHCAL_ HCAL energy weight factor and error', Z, 2)
      ENDIF
C
      JPHCO = IW(NAPHCO)
C
C
   20 TH = RTABL(JPHCO,NPHCO,JPHCTH)
      PH = RTABL(JPHCO,NPHCO,JPHCPH)
C
      EN = RTABL(JPHCO,NPHCO,JPHCEC) * Z(1)
      IF(EN.LT.1.E-9)EN=1.E-9
C
      SIGEN = Z(2)**2 * EN
      SIGTH = 0.
      SIGPH = 0.
C
      END
#endif
