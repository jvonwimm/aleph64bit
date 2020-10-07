      INTEGER FUNCTION ADBRUN ( DET,IROW,IPERIOD,IRUN,ISETUP)
C -----------------------------------------------------------
C - F.Ranjard - 901018
CKEY ALEF ADBR PERIOD
C! get elements of an ADBR period
C
C - Input  : DET      / A = CHARACTER*2 variable to define the det:
C                           'BP VD IT TP EC LC SA HC MU TR GE DB SI BE'
C            IRUN     / I = run #
C - Output : IROW     / I = ADBR row # valid for this run
C            IPERIOD  / I = period # of this row
C            ISETUP   / I = setup code
C                           = 0 if DET does not exist
C            ADBRUN   / I = 1st run of the period
C                           = 0 if no bank or row # does not exist
C
C -------------------------------------------------------------
#ifndef DOC
      CHARACTER*2 DET
#include "bcs.h"
#include "adbrjj.h"
#include "adbrlist.h"
      DATA NADBR / 0/
#include "bmacro.h"
C ------------------------------------------------------------
C - 1st entry : get ADBR bank from data base
#include "gtadbr.h"
C
C - return row# valid for a given run
      IR1 = ITABL(JADBR,1,JADBFR)
      IF (IRUN.LT.IR1) GOTO 998
      IR2 = ITABL(JADBR,LADBR,JADBFR)
      IF (IRUN.EQ.IR1) THEN
         IROW = 1
      ELSEIF (IRUN.GE.IR2) THEN
         IROW = LADBR
      ELSE
         DO 10 I=2,LADBR
            IR2 = ITABL(JADBR,I,JADBFR)
            IF (IRUN.GE.IR1 .AND.IRUN.LT.IR2) THEN
               IROW = I-1
               GOTO 20
            ENDIF
            IR1 = IR2
 10      CONTINUE
         GOTO 998
      ENDIF
C
C - return content of the row
 20   CONTINUE
      IPERIOD = ITABL(JADBR,IROW,JADBPN)
      ISETUP = 0
      IF (JDETJJ.GT.0) ISETUP = ITABL(JADBR,IROW,JDETJJ)
      ADBRUN = ITABL(JADBR,IROW,JADBFR)
      RETURN
C
C - period not found
 998  ADBRUN = 0
      END
#endif
