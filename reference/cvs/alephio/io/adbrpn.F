      INTEGER FUNCTION ADBRPN ( DET,IROW,IPERIOD,IRUN,ISETUP)
C -----------------------------------------------------------
C - F.Ranjard - 901018
CKEY ALEF ADBR PERIOD
C! get elements of an ADBR period
C
C - Input  : DET      / A = CHARACTER*2 variable to define the det:
C                           'BP VD IT TP EC LC SA HC MU TR GE DB SI BE'
C            IPERIOD  / I = period #
C - Output : IROW     / I = ADBR row # valid for this period
C            IRUN     / I = 1st run of this period
C            ISETUP   / I = detector setup code
C                           = 0 if DET does not exist
C            ADBRPN   / I = valid period #
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
C - return row# valid for a given period and the 1st run
      IP1 = ITABL(JADBR,1,JADBPN)
      IF (IPERIOD.LT.IP1) GOTO 998
      IP2 = ITABL(JADBR,LADBR,JADBPN)
      IF (IPERIOD.EQ.IP1) THEN
         IROW = 1
      ELSEIF (IPERIOD.GE.IP2) THEN
         IROW = LADBR
      ELSE
         DO 10 I=2,LADBR
            IP2 = ITABL(JADBR,I,JADBPN)
            IF (IPERIOD.GE.IP1 .AND.IPERIOD.LT.IP2) THEN
               IROW = I-1
               GOTO 20
            ENDIF
            IP1 = IP2
 10      CONTINUE
         GOTO 998
      ENDIF
C
C - return content of the row
 20   CONTINUE
      IRUN = ITABL(JADBR,IROW,JADBFR)
      ISETUP = 0
      IF (JDETJJ.GT.0) ISETUP = ITABL(JADBR,IROW,JDETJJ)
      ADBRPN = ITABL(JADBR,IROW,JADBPN)
      RETURN
C
C - period not found
 998  ADBRPN = 0
      END
#endif
