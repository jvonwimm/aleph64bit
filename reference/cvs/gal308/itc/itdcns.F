*DK itdcns
      SUBROUTINE ITDCNS
C.
C...ITDCNS  2.11  930721  18:03                          R.Beuselinck
C.
C!  Get constants controlling drift relation and resolution.
C.
C.  Called by: ITIRUN                                  from this .HLB
C.      Calls: NAMIND                                  from BOS77
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "iocom.h"
#include "jobcom.h"
#include "itelec.h"
#include "itparc.h"
#include "itresc.h"
C
      EXTERNAL NAMIND
#include "idrpjj.h"
#include "ieffjj.h"
#include "irrfjj.h"
#include "bmacro.h"
C
C--  Get constants for parametrization and efficiency.
C--
      IND = NAMIND('IEFF')
      IND = IW(IND)
      HWEFIT = RTABL(IND,1,JIEFMX)
C
C--  Get resolution parameters from database.
C--
      IND = IW(NAMIND('IRRF'))
      NR = LROWS(IND)
      IF (NR.NE.8)
     +  CALL ALTELL('ITDCNS: Wrong number of layers in IRRF',0,'STOP')
      DO 40 I=1,NR
        JROW = KROW(IND,I)
        LAY = I
        DO 35 J=1,5
          RESITP(J,I) = RW(JROW+JIRRCP+J-1)
          RESITN(J,I) = RW(JROW+JIRRCN+J-1)
   35   CONTINUE
   40 CONTINUE
      IND = IW(NAMIND('IDRP'))
      NR = LROWS(IND)
      IF (NR.NE.8)
     +  CALL ALTELL('ITDCNS: Wrong number of layers in IDRP',0,'STOP')
      DO 50 I=1,NR
        JROW = KROW(IND,I)
        LAY = I
        TFINIT(LAY) = RW(JROW+JIDRTO)
        DVELIT(LAY,1) = RW(JROW+JIDRCO)
        DVELIT(LAY,2) = RW(JROW+JIDRCO+1)
        DVELIT(LAY,3) = RW(JROW+JIDRCO+2)
        DVELIT(LAY,4) = RW(JROW+JIDRCO+3)
        DVELIT(LAY,5) = RW(JROW+JIDRCO+4)
   50 CONTINUE
C
C--  Initialise interpolation table for drift time.
C--
      CALL ITDTGO
C
      END
#endif
