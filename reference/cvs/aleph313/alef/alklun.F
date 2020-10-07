      INTEGER FUNCTION ALKLUN (LUVER,LULMD)
C -------------------------------------------------------------------
CKEY ALEF LUND KLUN / USER
C - B.Bloch-Devaux 900926
C! Build Lund run header KLUN
C - Input  :  LUVER   = JETSET version number (packed as 703 for 7.3)
C             LULMD   = JETSET last date of change (as YYMMDD)
C - Output :  ALKLUN  = KLUN bank index
C                       0 means not enough space to book the bank
C  ---------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "klunjj.h"
#include "bmacro.h"
C --------------------------------------------------------------------
C - Book the bank 'KLUN'
      CALL AUBOS ('KLUN',0,LMHLEN+LKLUNA,JKLUN,IGARB)
      IF (JKLUN.EQ.0) GOTO 999
      IW(JKLUN+LMHCOL) = LKLUNA
      IW(JKLUN+LMHROW) = 1
      CALL BKFMT ('KLUN','I')
C
C - fill the  row
      KKLUN = JKLUN + LMHLEN
      IW(KKLUN+JKLUVN) = LUVER
      IW(KKLUN+JKLULM) = LULMD
C
 999  CONTINUE
      ALKLUN = JKLUN
      END
#endif
