      SUBROUTINE X2IRUN (IRUN,FHIST,IRET)
C
C --------------------------------------------------
C! Initial run routine for level2 trigger.
C
C   Author: T. Medcalf  10/9/87
C
C? Call routines to set up constants, Bos bank names,
C? and look-up tables.
C - Input  :  IRUN / I     = current run number
C             FHIST / L    = histogram flag
C
C - Output :  IRET / I     = return flag
C                            0 means  OK
C                            2 FATAL error in X2LUTS
C --------------------------------------------------
#ifndef DOC
      SAVE
#include "x2cons.h"
      INTEGER IRUN,IRET
      LOGICAL FHIST
C --------------------------------------------------
C
      IX2RUN = IRUN
      IX2HIS = 0
      IF (FHIST) IX2HIS = 1
      IRET = 0
C
C - read data card X2RU if any
      CALL X2DFCD
C
C - create name-indices and set working bank indices to 0
      CALL X2NAMI
C
C - Create look up tables used by the electronics
      CALL X2LUTS (IRET)
C
      RETURN
      END
#endif
