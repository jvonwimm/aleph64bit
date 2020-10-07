      SUBROUTINE ECDEBG
C----------------------------------------------------------------------
C!  - PRINT BOS BANK CONTENTS AFTER PROCESSING
C!
C!        INPUT ESDA,ECLU,ETP1,EFET,ECT1/2/3
C?
C!======================================================================
#ifndef DOC
      EXTERNAL NPRNT
C
#include "rparac.h"
#include "rflags.h"
#include "bcs.h"
#include "rjobco.h"
C
      IF (JDBDRF(JULEC) .GE. 3) THEN
      INDEX=NPRNT('ESDA',0)
      INDEX=NPRNT('ECLU',0)
      INDEX=NPRNT('ETP1',0)
      INDEX=NPRNT('ESTO',0)
      INDEX=NPRNT('EFET',0)
      INDEX=NPRNT('ECT1',0)
      INDEX=NPRNT('ETC2',0)
      INDEX=NPRNT('ERL3',0)
      INDEX=NPRNT('TGFT',0)
C
      ENDIF
      RETURN
      END
#endif
