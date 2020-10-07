      INTEGER FUNCTION ICASWT(IADD,IRUNC)
C----------------------------------------------------------------------
C!  - Switch dead storey address in End-caps
C!
C!   Author   :- M.N. Minard                3-SEP-1992
C!
C!     IADD  = original encoded address from EDDB
C!     IRUNC = Run number of the current run
C?
C!======================================================================
#include "rcurnt.h"
#ifndef DOC
C     Test Run number for switching procedure
      ICASWT = 0
      IF (FMCRUN.OR.IRUNC.GT.18000) THEN
        ICASWT = IADD
      ELSE
C
C-    Chech it is an endcap address
C
        JVAL = IBITS(IADD,16,8)
        IVAL = IBITS(IADD,2,9)
        KVAL = IBITS(IADD,26,2)
        CALL ECMOD(IVAL,JVAL,IMOD)
        IF ( IMOD.GT.12.AND.IMOD.LT.25) THEN
          ICASWT = IADD
        ELSE
C
C-    Switching procedure
C
          JVAL1 = JVAL
          IF(JVAL.GT.178) THEN
            JVAL1= 228-JVAL
          ENDIF
          IF (JVAL1.LE.8) MOD = 8
          IF (JVAL1.GT.8.AND.JVAL1.LE.24) MOD = 16
          IF (JVAL1.GT.24.AND.JVAL1.LE.40) MOD = 24
          IF (JVAL1.GT.40) MOD=32
          IMOD2 = MOD/2
          IVLL = (IVAL-IMOD2-1)/MOD
          IF(IVLL.LT.0) IVLL =0
          IF((IVAL-IMOD2).LT.0) IVLL=-1
          IRES = (IVAL-IMOD2)-IVLL*MOD
          INVAL = IMOD2 +IVLL*MOD+(MOD+1-IRES)
          IF(INVAL.LE.0) INVAL=INVAL+12*MOD
          IF(INVAL.GT.12*MOD)INVAL = INVAL-12*MOD
          ICASWT= INVAL*(2**2)+JVAL*(2**16)+KVAL*(2**26)
          JVAL = IBITS(ICASWT,16,8)
          IVAL = IBITS(ICASWT,2,9)
          KVAL = IBITS(ICASWT,26,2)
        ENDIF
      ENDIF
      RETURN
      END
#endif
