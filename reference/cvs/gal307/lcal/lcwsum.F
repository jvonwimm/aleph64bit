*DK lcwsum
      SUBROUTINE LCWSUM
C--------------------------------------------------------------
C! - Job summary for LCal
C. - J.Dines Hansen & P.Hansen - 860417
C. - Modified P.Hansen - 950202
C. - Called by  ASCRUN                           from this .HLB
C -----------------------------------------------
#ifndef DOC
#include "iocom.h"
#include "jqcom.h"
#include "lccomc.h"
#include "lcnamc.h"
C ------------------------------------------------------------
      KWS = IW(NALCWS)
      IF(KWS .LE. 0)                   GOTO 999
      WRITE(LOUTIO,1000)
      WRITE(LOUTIO,1100) IW(KWS+LMHLEN+1)
      EVTS = FLOAT(IW(KWS+LMHLEN+1))
      EPAR = FLOAT(IW(KWS+LMHLEN+5)+IW(KWS+LMHLEN+6))
      ETOW = FLOAT(IW(KWS+LMHLEN+7)+IW(KWS+LMHLEN+8))
      EWIR = FLOAT(IW(KWS+LMHLEN+9)+IW(KWS+LMHLEN+10))
      ETRI = FLOAT(IW(KWS+LMHLEN+3)+IW(KWS+LMHLEN+4))
      IF(IW(KWS+LMHLEN+1).GE.1) THEN
         WRITE(LOUTIO,1400) EPAR/EVTS
         WRITE(LOUTIO,1500) ETOW/EVTS
         WRITE(LOUTIO,1600) EWIR/EVTS
      ENDIF
      WRITE(LOUTIO,1200) LCBHTR,IW(KWS+LMHLEN+2)
      IF(IW(KWS+LMHLEN+2).GE.1) THEN
         ET = ETRI/FLOAT(IW(KWS+LMHLEN+2))
         WRITE(LOUTIO,1300) ET
      ENDIF
  999 RETURN
C
 1000 FORMAT(//,'1+++ LCAL SUMMARY +++')
 1100 FORMAT(' No. of events with particles in LCal.',29X,I10)
 1200 FORMAT(' No. of events above Bhabha-trigger threshold'
     *       ,I10,' MeV is',I10)
 1300 FORMAT('   Average trigger  energy in LCAL',F10.1,' MeV')
 1400 FORMAT('   Average particle energy in LCAL',F10.1,' MeV')
 1500 FORMAT('   Average tower    energy in LCAL',F10.1,' MeV')
 1600 FORMAT('   Average wire     energy in LCAL',F10.1,' MeV')
      END
#endif
