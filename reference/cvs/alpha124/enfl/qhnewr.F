      SUBROUTINE QHNEWR
C-----------------------------------------------------------------------
C! Get HCAL trigger thresholds and noisy towers for each new run
C
C  Gerardo Ganis -- 1989
C-----------------------------------------------------------------------
#ifndef DOC
#include "cuhnto.h"
#include "qcde.h"
#include "qhac.h"
      PARAMETER( NRUDAT= 4000 )
#include "qmacro.h"
      DATA KRUNOLD /0/
C-----------------------------------------------------------------------
C
      IF(KRUN.EQ.KRUNOLD) RETURN
      KRUNOLD= KRUN
C
C----- Decode HCAL wires trigger thresholds
      CALL QURHTR
C
C----- Read HCAL noise ( for data only )
      IF(KRUN.GT.NRUDAT) THEN
        CALL QURHNT(KRUN,IER)
        IF(IER.NE.0) THEN
          WRITE (IW(6),101) IER,KRUN
        END IF
      ELSE
        NNTO = 0
      END IF
101   FORMAT(/,1X,'_QURHNT_: Problems reading HNTO ',I4,/,
     &         1X,' /CUHNTO/ not modified _ Run ',I6,/)
C
999   CONTINUE
      RETURN
      END
#endif
