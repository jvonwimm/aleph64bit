      SUBROUTINE LPRSUM
C--------------------------------------------------------------------
C! Print general event statistics
C!
C!  Author       :- P.H.Hansen 89/01/02
C!
C!  Input:       'LSTA'
C!
C--------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "rstatr.h"
      PARAMETER (LLEN=50)
      PARAMETER (KLEN=5)
      CHARACTER*(LLEN) TLRUN(KLEN)
      EXTERNAL NAMIND
      DATA TLRUN /
     $ 'Number of events with Lcal clusters...............',
     $ 'Energy/event module 1.............................',
     $ 'Energy/event module 2.............................',
     $ 'Energy/event module 3.............................',
     $ 'Energy/event module 4.............................'/
C--------------------------------------------------------------------
C
C Normalise energies to number of events
      KLSTA = IW(NAMIND('LSTA'))
      KLS   = KLSTA + LMHLEN
      IF(RW(KLS+1).GT.0.1) THEN
         DO 10 I=1,4
            RW(KLS+I+1) = RW(KLS+I+1)/RW(KLS+1)
   10    CONTINUE
      ENDIF
C Print
      WRITE(LOUTRL,500)
 500  FORMAT(1X,80('!'),/,1X,
     &'Lcal reconstruction summary :',
     &//)
      WRITE(LOUTRL,'(1X,A,F10.2)') (TLRUN(I),RW(KLS+I),I=1,KLEN)
      WRITE(LOUTRL,510)
  510 FORMAT(1X,'--------------------------------------------',/)
      END
#endif
