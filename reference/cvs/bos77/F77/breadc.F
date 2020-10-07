      SUBROUTINE BREADC
      COMMON/BCS/IW(1000)
      EQUIVALENCE (IPR,IW(4))
#include "param.h"
      IW(3)=0
      IF(IPR.EQ.1.AND.IW(LUP).NE.0) WRITE(IW(LUP),101)
   10 IND=MREADC(IW)
      IF(IW(3).EQ.0) IW(3)=IND
      IF(IND.NE.0) GOTO 10
  100 RETURN
  101 FORMAT('0BREADC-----  NAME........NR  +--',11('------'),'--+')
      END
