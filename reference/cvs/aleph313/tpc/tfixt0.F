      FUNCTION TFIXT0(IRUN,IEVT)
C--------------------------------------------------------------------
C! Correct for 1997 hardware problem which shifted T0 by several ns.
C! during periods of high luminosity at start of fill.
C!
C!    Author:    I. Tomalin    3-10-97
C!
C! INPUT:
C!   INTE IRUN: Run number
C!   INTE IEVT: Event number
C! OUTPUT:
C!   REAL TFIXT0: Offset which should be applied to TPC t0 in ns.
C!-------------------------------------------------------------------
      SAVE
      DATA LRUN/0/
C
#include "tft0jj.h"
#include "bcs.h"
#include "bmacro.h"
C!-------------------------------------------------------------------
       TFIXT0 = 0.0
C
C Only bother looking at data in dodgy period.
       IF (IRUN.GE.43700.AND.IRUN.LE.44200) THEN
C
C Access database bank containing corrections.
        IF (IRUN.NE.LRUN) THEN
C If new run then read it from the database.
          IF (LRUN.GT.0) KTFT0 = NDROP('TFT0',LRUN)
          LRUN = IRUN
          LDBAS = JUNIDB(0)
          KTFT0 = MDARD(IW,LDBAS,'TFT0',IRUN)
        ELSE
C Else just relink it.
          KTFT0 = NLINK('TFT0',IRUN)
        ENDIF
C
        IF (KTFT0.GT.0) THEN
C Loop over periods needing different corrections in this run.
          DO 10 I = 1,LROWS(KTFT0)
            IF (IEVT.LE.ITABL(KTFT0,I,JTFTEV)) THEN
              TFIXT0 = RTABL(KTFT0,I,JTFTT0)
              GOTO 99
            END IF
   10     CONTINUE
        END IF
      END IF
C
   99 CONTINUE
      END
