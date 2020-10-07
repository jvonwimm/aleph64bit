      LOGICAL FUNCTION BADTRK(ITK, COSTH, NTPC, D0, Z0)
CKEY QSELEP/INTERNAL
C-------------------------------------------------------------------------------
C! Auxiliary to QSELEP
C  Function to do initial test of whether a track is good or bad.
C  Author                           I. Ten Have 27-Jul-1994
C If BAD  returns .TRUE.
C If GOOD returns .FALSE.
C-------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
      INTEGER ITK,NTPC
      REAL COSTH,D0,Z0
#include "qmacro.h"
C-------------------------------------------------------------------------------
C
      IF (ABS(QCT(ITK)).LE.COSTH.AND.KFRTNT(ITK).GE.NTPC.AND.
     @    ABS(QDB(ITK)).LE.D0.AND.ABS(QZB(ITK)).LE.Z0) THEN
        BADTRK = .FALSE.
      ELSE
        BADTRK = .TRUE.
      ENDIF
C
      RETURN
      END
#endif
