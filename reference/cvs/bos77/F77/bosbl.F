      SUBROUTINE BOSBL(JW)
C
C     PRINT IO-STATISTIC ON BANKS
C
      INTEGER JW(*)
      CHARACTER*4 CHAINT
#include "boscom.h"
#include "param.h"
#include "mparam.h"
C     ...
      IF(JW(LUP).LE.0) GOTO 100
      ID=JW(IOS)
      IF(ID.EQ.0) GOTO 100
      NN=MIN0(IW(IDNAM),JW(ID)/2)
      WRITE(JW(LUP),1001)
      WRITE(JW(LUP),1002) (CHAINT(IW(IDNAM+I)),JW(ID+2*I-1),JW(ID+2*I),
     +      I=1,NN)
  100 RETURN
 1001 FORMAT('0  ',5('NAME .....IN ...OUT',6X)/)
 1002 FORMAT(5(3X,A4,I8,I7,3X))
      END
