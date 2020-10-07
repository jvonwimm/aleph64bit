      INTEGER FUNCTION ADBROW (DET,IROW,IPERIOD,IRUN,ISETUP)
C -----------------------------------------------------------
C - F.Ranjard - 901018
CKEY ALEF ADBR
C! get elements of an ADBR ROW
C
C - Input  : DET      / A = CHARACTER*2 variable to define the det:
C                           'BP VD IT TP EC LC SA HC MU TR GE DB SI BE'
C            IROW     / I = ADBR row #
C - Output : IPERIOD  / I = period # corresponding to this row
C            IRUN     / I = 1st run of this period
C            ISETUP   / I = detector setup code
C                           = 0 if DET does not exist
C            ADBROW   / I = row #
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
C - return period # and run # of a given row
C
      IF (IROW.LT.1 .OR. IROW.GT.LADBR) GOTO 998
      IPERIOD = ITABL (JADBR,IROW,JADBPN)
      IRUN    = ITABL (JADBR,IROW,JADBFR)
      ISETUP  = 0
      IF (JDETJJ.NE.0) ISETUP  = ITABL (JADBR,IROW,JDETJJ)
      ADBROW  = IROW
      RETURN
C
 998  ADBROW =0
      END
#endif
