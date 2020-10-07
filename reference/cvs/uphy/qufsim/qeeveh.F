      SUBROUTINE QEEVEH(ECM,IER)
C----------------------------------------------------------------
C! Get event cms energy stored in EVEH bank
C  Output -  ECM  cms energy
C            IER= 0 if ok
C               = 1 if EVEH does not exist
C
C  G.Ganis   21 Mar 1991
C----------------------------------------------------------------
#include "qcde.h"
#include "qhac.h"
C
      IER= 0
C
      ECM= 0.
      IEVEH= NLINK('EVEH',0)
      IF(IEVEH.NE.0) THEN
        ECM= FLOAT(IW(IEVEH+13))/1000000
      ELSE
        IER= 1
      END IF
C
      RETURN
      END
