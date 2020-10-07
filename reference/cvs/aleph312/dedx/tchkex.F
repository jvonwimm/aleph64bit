      LOGICAL FUNCTION TCHKEX(IRUN)
C
C-----------------------------------------------------------------------
C! Check for existance of good dE/dx calibration constants on the
C  database
CKEY DEDX TPC CALIBRATION
C
C    R. Johnson     27-4-90
C    Modified:W. Wiedenmann 31-08-93 use new banks TCGX/TCSX
C
C
C    Input:    IRUN      /I        Current run number
C    Output:   TCHKEX    /L        .TRUE. if correct calibration
C                                  exists, otherwise .FALSE.
C
C    Correct calibration may not exist either because the run has not
C    yet been calibrated, or it is a DUCK run.
C    in this case the run is not included in TCGX/TCSX banks, the global
C    calibration is set to 0.
C
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "tpgpar.h"
      DIMENSION SNR(LTSECT)
C
      TCHKEX=.FALSE.
      CALL TDEDXR(IRUN,GNR,SNR,IER)
      IF (IER.NE.0) RETURN
      IF (GNR.EQ.0) RETURN
C
      TCHKEX=.TRUE.
      RETURN
      END
#endif
