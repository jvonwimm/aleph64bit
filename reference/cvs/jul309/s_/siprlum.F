      SUBROUTINE SIPRLUM
C----------------------------------------------------------------------
C!  - SICAL LUMI  Summary
C!
C!   Author   :- B. Bloch                     14/02/93
C!              Modified for Bunch train mode October 1994 :
C!              Print Luminosity summary per wagon in the train
C!
C!  Input:       LUMI COUNTERS COMMON
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "rcurnt.h"
      COMMON/SILUMC/NSILEV(25,4)
      INTEGER ALGTWA
      EXTERNAL ALGTWA
      PARAMETER (LLEN=50)
      PARAMETER (KLEN=16)
      CHARACTER*(LLEN) TSRUN(KLEN)
      EXTERNAL NAMIND
      DATA TSRUN /
     $  'Number of events with Sical data analysed.........',
     $  'Events with at least one cluster per side.........',
     $  'Events after Energy A> EA.........................',
     $  'Events after Energy B> EB.........................',
     $  'Events after Energy A + Energy B > Es.............',
     $  'Method 1 events after tight side inner R cut......',
     $  'Method 1 events after tight side outer R cut......',
     $  'Method 1 events after loose side inner R cut......',
     $  'Method 1 events after loose side outer R cut......',
     $  'Method 1 events after Delta Phi > min ............',
     $  'Method 1 events after Delta Phi < max = final.....',
     $  'Method 3 vertical selection  .....................',
     $  'Method 5 horizontal selection  ...................',
     $  'Method 2 events after tighter side inner R cut....',
     $  'Method 4 vertical selection  .....................',
     $  'Method 6 horizontal selection  ...................'/
C--------------------------------------------------------------------
C
C Print
      WRITE(LOUTRL,500)
      NWAG = ALGTWA(IRUNRC)
      IF ( NWAG.EQ.1) THEN
         WRITE(LOUTRL,501)
      ELSE IF ( NWAG.EQ.2) THEN
         WRITE(LOUTRL,502)
      ELSE IF ( NWAG.EQ.3) THEN
         WRITE(LOUTRL,503)
      ELSE IF ( NWAG.EQ.4) THEN
         WRITE(LOUTRL,504)
      ENDIF
      DO 20 I= 1,KLEN
         NTOT = 0
         DO 21 K= 1,NWAG
  21     NTOT = NTOT+NSILEV(I,K)
         WRITE(LOUTRL,509)  TSRUN(I),(NSILEV(I,J),J=1,NWAG),NTOT
  20  CONTINUE
      WRITE(LOUTRL,510)
  999 RETURN
  500 FORMAT(1X,80('!'),/,1X,'Sical Luminosity summary :',//)
  501 FORMAT(1X,50X,' Bunch # 1 Total   ')
  502 FORMAT(1X,50X,' Bunch # 1 Bunch # 2 Total   ')
  503 FORMAT(1X,50X,' Bunch # 1 Bunch # 2 Bunch # 3 Total   ')
  504 FORMAT(1X,50X,' Bunch # 1 Bunch # 2 Bunch # 3 Bunch # 4 Total   ')
  509 FORMAT(1X,A,5I10)
  510 FORMAT(1X,'--------------------------------------------',/)
      END
#endif
