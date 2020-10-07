      SUBROUTINE E4RES2(XX,YY,A,B,C,IERROR)
C----------------------------------------------------------------------
C!  - R2 ESTIMATOR : GIVE INTERSECTION OF PLAN WITH EXTRAPOLATED TRACK
C!      JOB FOR R2 ESTIMATOR CALCULATION
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C?
C!======================================================================
#ifndef DOC
#include "rlunit.h"
#include "rflags.h"
#include "rparac.h"
      DIMENSION XX(2),YY(2)
      ERROR=0
      DET=XX(1)-XX(2)
      IF(DET.EQ.0)THEN
        ERROR=1
        RETURN
        ENDIF
      A=(YY(1)-YY(2))/DET
      B=-1
      C=YY(2)-A*XX(2)
      IF(JDBDRF(JULEC).GT.8)
     &WRITE(LDEBRL,*)'EQ PLAN AX+BY+C=0',A,B,C
      RETURN
      END
#endif
