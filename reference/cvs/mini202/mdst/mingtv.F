      INTEGER FUNCTION MINGTV(DUMMY)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Mini-DST version number used to produce current data.
C
C     This is obtained from the event bank DVRS or the run bank RHAH by
C     a call to ALVSN.
C     It can be overrode by the MVOK data card.
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "dvrsjj.h"
C
      DIMENSION IPV(10),IAV(10)
      SAVE LRUN,LEVT,MVER
      DATA LRUN,LEVT,MVER / -999,-999,0 /
C
#include "bmacro.h"
C
C++   See if we have already found version for this event.
C
      CALL ABRUEV(IRUN,IEVT)
      IF (IRUN.EQ.LRUN .AND. IEVT.EQ.LEVT) GOTO 1000
      LRUN = IRUN
      LEVT = IEVT
C
C++   Priority given to overriding version number with MVOK card.
C
      KMVOK = NLINK('MVOK',0)
      IF (KMVOK.GT.0 .AND. IW(KMVOK).GE.1) THEN
         MVER = IW(KMVOK+1)
         GOTO 1000
      ENDIF
C
C++   Next look for DVRS bank - default method for versions from 9.0.
C
      KDVRS = NLINK('DVRS',0)
      IF (KDVRS.GT.0) THEN
         MVER = ITABL(KDVRS,1,JDVRMV)
         GOTO 1000
      ENDIF
C
C++   Lastly look in RHAH history.
C
      CALL ALVSN(ITYP,IPV,IAV,IYR)
      MVER = IPV(5)
C
 1000 MINGTV = MVER
C
      RETURN
      END
#endif
