      SUBROUTINE BCLAST(LUN)
*     write the final bank on unit LUN ...
#include "boscom.h"
      LUNA=LUN
    5 IND=NAMIND('$PTS')+1
   10 IND=IW(IND-1)
      IF(IND.EQ.0) GOTO 100
      LUNB=IW(IND-2)
      IF(LUNB.EQ.LUNSEL) GOTO 10
      IF(LUNA.NE.0.AND.LUNA.NE.LUNB) GOTO 10
C     CALL BOSDP(IW)
      ISW=NSWAP('$PTS',LUNB,'$PTS',0)
C     CALL BOSDP(IW)
      IF(ISW.EQ.0) GOTO 5
      IV=IW(IND+IW(IND))
      ISW=NBANK('$PTS',0,IV)
      IF(ISW.NE.0) CALL BWRSB(IW,LUNB,'$PTS',0)
      CALL BWRITE(IW,LUNB,'0')
      ISW=NDROP('$PTS',0)
      GOTO  5
  100 RETURN
      END
