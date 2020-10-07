      INTEGER FUNCTION ALKJOB (IDBV,IDBD)
C ------------------------------------------------------------------
C - B.Bloch-Devaux 880720
C! Book and fill kine job header KJOB
C - Input :  IDBV   = DATA BASE version #
C            IDBD   = DATA BASE last mod date
C - Output : ALKJOB = KJOB bank index
C                     0 means not enough space to book the bank
C
C ------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "kjobjj.h"
#include "version.h"
#include "bmacro.h"
C ----------------------------------------------------------------------
C - Book the 'KJOB' bank
      CALL AUBOS ('KJOB',0,LMHLEN+LKJOBA,JKJOB,IGARB)
      IF (JKJOB.EQ.0) GOTO 999
      IW(JKJOB+LMHCOL) = LKJOBA
      IW(JKJOB+LMHROW) = 1
      CALL BKFMT ('KJOB','7I')
C
C - Get date and time
C
      CALL DATIME(JDAT,JTIM)
C
C - Fill 'KJOB'
C
      KKJOB = JKJOB + LMHLEN
      IW(KKJOB+1) = JDAT
      IW(KKJOB+2) = JTIM
      IALV = ALEVER*10.
      IW(KKJOB+3) = IALV
      IW(KKJOB+4) = IDBV
      IW(KKJOB+5) = IDBD
C
 999  CONTINUE
      ALKJOB = JKJOB
      END
#endif
