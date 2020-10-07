      SUBROUTINE RWJUBW
C---------------------------------------------------------------------
C - P.Comas, 27-APR-94
C! Store bunch number identification used for reconstruction in EVEH
C---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "evehjj.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C Read EVEH
      KEVEH=IW(NAMIND('EVEH'))
      IF (KEVEH.LE.0) GOTO 999

C NO buch trains for MC data and data < '95
      IRUN = IW(KEVEH+JEVERN)
      IF (IRUN.LE.35000) GOTO 999

C Get the bunch id
      KBUNCH = IW(KEVEH+JEVEM4)

C Get the combined bunch number and quality flag
      KCOM = IBITS (KBUNCH,0,3)
      KQUA = IBITS (KBUNCH,12,2)

C Write bunch number and quality flag that will be used for reconstruction
C in the EVEH
      KBUNCH = IOR(KBUNCH,ISHFT(KCOM,27))
      KBUNCH = IOR(KBUNCH,ISHFT(KQUA,30))

C Fill word 10
      IW(KEVEH+JEVEM4) = KBUNCH


999   RETURN
      END
#endif
