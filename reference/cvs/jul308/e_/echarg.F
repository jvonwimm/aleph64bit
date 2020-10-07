      SUBROUTINE ECHARG(ICL,ICHR)
C----------------------------------------------------------------------
C!  - CHECK IF A CLUSTER IS 'CHARGED'
C!
C!  AUTHOR   : D. PALLIN
C!  MODIFIED :
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "eclujj.h"
#include "erl3jj.h"
#include "ect1jj.h"
#include "ecnamc.h"
#include "bmacro.h"
C
      KECT1 = IW(NAECT1)
      KERL3=IW(NAERL3)
      ICHR=0
      IREL=ITABL(KECT1,ICL,JECTER)
   11 IF (IREL.NE.0)  THEN
        ITK=ITABL(KERL3,IREL,JERLET)
        CALL ESLAS(ICL,ITK,IKEPT)
        IF(IKEPT.EQ.1)THEN
           ICHR=1
           GO TO 10
        ENDIF
C
        IREL=ITABL(KERL3,IREL,JERLER)
        GO TO 11
      ENDIF
   10 CONTINUE
      RETURN
      END
#endif
