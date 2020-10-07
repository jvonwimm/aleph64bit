      SUBROUTINE SIPRSUM
C----------------------------------------------------------------------
C!  - SICAL Job Summary
C!
C!   Author   :- B. Bloch                     24/08/92
C!              Modified for Bunch train mode October 1994 :
C!              Print reconstruction summary per wagon in the train
C!
C!  Input:       'SSTA'
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
      PARAMETER (LLEN=50)
      PARAMETER (KLEN=9)
      CHARACTER*(LLEN) TSRUN(KLEN)
      EXTERNAL NAMIND
      DATA TSRUN /
     $  'Number of events with Sical data..................',
     $  'Energy/event module A.............................',
     $  'Energy/event module B.............................',
     $  'Npad  /event module A.............................',
     $  'Npad  /event module B.............................',
     $  'Cluster Energy/event module A.....................',
     $  'Cluster Energy/event module B.....................',
     $  'NCluster/event module A...........................',
     $  'NCluster/event module B...........................'/
#include "bmacro.h"
C--------------------------------------------------------------------
C
C Normalise quantities to number of events
      KSSTA = IW(NAMIND('SSTA'))
      IF ( KSSTA.gt.0) THEN
         DO 11 NWA = 1,LROWS(KSSTA)
            KLS = KROW(KSSTA,NWA)
      IF(RW(KLS+1).GT.0.1) THEN
        DO 10 I=1,8
          RW(KLS+I+1) = RW(KLS+I+1)/RW(KLS+1)
   10   CONTINUE
      ENDIF
  11     CONTINUE
      ENDIF
C Print
      WRITE(LOUTRL,500)
      WRITE(LOUTRL,501)
      NWA = LROWS(KSSTA)
      KLS = KSSTA + LMHLEN
      DO 20 I= 1,KLEN
         WRITE(LOUTRL,502) TSRUN(I),(RW(KLS+KLEN*(J-1)+I),J=1,NWA)
  20  CONTINUE
      WRITE(LOUTRL,510)
  999 RETURN
  500 FORMAT(1X,80('!'),/,1X,'Sical reconstruction summary :',//)
  501 FORMAT(1X,50X,'Bunch # 1 Bunch # 2 Bunch # 3 Bunch # 4')
  502 FORMAT(1X,A,4F10.2)
  510 FORMAT(1X,'--------------------------------------------',/)
      END
#endif
