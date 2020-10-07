      INTEGER FUNCTION  ALGTLU (LUVER,LULMD)
C -------------------------------------------------------------------
CKEY ALEF LUND VERSION KLUN KINGAL / USER
C - B.Bloch-Devaux 900926
C! Finds the JETSET version number used for generated events
C   Returns  ALGTLU = 1  if the file WAS  generated using JETSET
C                   = 0        #     WAS NOT            #
C - Output :  LUVER   = JETSET version number ( 703 FOR 7.3 VERSION )
C             LULMD   = JETSET date of last modification
C                          (900918 is 18 September 1990)
C Input banks : KLUN ,  KRUN
C ----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "klunjj.h"
#include "krunjj.h"
C   Generators below this ID never used JETSET!
      PARAMETER (IGEN = 3002)
#include "bmacro.h"
C ----------------------------------------------------------------------
        LUVER =0
        LULMD =0
        ALGTLU=0
C  Get Run header bank
        JKRUN=NLINK('KRUN',0)
        IF (JKRUN.EQ.0) GO TO 999
        ILUND =ITABL(JKRUN,1,JKRUGI)
        IF (ILUND.GT.IGEN) GO TO 999
        ALGTLU=1
        LUVER =603
        LULMD =870210
        JKLUN=NLINK('KLUN',0)
        IF (JKLUN.EQ.0) GO TO 999
        LUVER =ITABL(JKLUN,1,JKLUVN)
        LULMD =ITABL(JKLUN,1,JKLULM)
C
 999    RETURN
        END
#endif
