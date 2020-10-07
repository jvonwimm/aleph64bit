      SUBROUTINE BCLASR(ICLASS)

*     define bit for input

#include "boscom.h"
#include "bitconst.h"
      IF(ICLASS.LE.0.OR.ICLASS.GT.30) GOTO 100
      MASKR=IOR(MASKR,IBITC(ICLASS))
  100 RETURN
      END
