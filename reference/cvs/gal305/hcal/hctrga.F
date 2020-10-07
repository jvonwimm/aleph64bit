*DK hctrga
      SUBROUTINE HCTRGA
C --------------------------------------------
C
C! Implements the module FORM TOWER TRIGGER SIGNAL
C!
C!
C!      Author     : G.Zito 86/05/21
C!      Modified by: F.Ranjard 89/02/10
C!
C!      Input bank  : HTHT  McHcStoreys
C!      Output bank : HTTR  McHcTowerTrigSignal
C!
C!
C!   -Called by : HCDIGI
C!   -Calls     : none
C -----------------------------------------
#ifndef DOC
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
#include "hccoun.h"
#include "hcnamc.h"
#include "httrjj.h"
#include "hthtjj.h"
#include "hctrig.h"
#include "bmacro.h"
C ------------------------------------------------------------
      JHTHT = IW(NAHTHT)
      NHTHT = LROWS (JHTHT)
      IF (NHTHT.EQ.0) RETURN
      JHTTR = IW(NAHTTR)
C
C          Scan the table McHcStoreys adding to trigger signal
C
      KHTHT = JHTHT + LMHLEN
      N1 = NHCETR
      N2 = N1 + NHCBTR
      N3 = N1 + NHCBTR/2
      DO 10 I = 1, NHTHT
         NTHS = IW(KHTHT+3)
         NFIS = IW(KHTHT+2)
C
C          Trigger signals stored following Putzer description
C
         IF (NTHS.LE.N1) THEN
            IPOS = (NFIS-1)*NHCETR + NTHS
         ELSE IF (NTHS.LE.N3) THEN
            IPOS = MHCETR + (NFIS-1)*(NHCBTR/2) + NTHS
         ELSE IF (NTHS.LE.N2) THEN
            IPOS = MHCETR + (NHCBPR+NFIS-2)*(NHCBTR/2)+NTHS
         ELSE
            IPOS = MHCETR + MHCBTR + (NFIS-1)*NHCETR + NTHS
         ENDIF
         KHTTR = KROW (JHTTR,IPOS)
         IW(KHTTR+1) = IW(KHTTR+1) + NINT (RW(KHTHT+4)*1000.)
         KHTHT = KHTHT + LHTHTA
   10 CONTINUE
C
C
      IF(FHCDB1)THEN
         WRITE(LOUTIO,500)
         KHTTR = JHTTR + LMHLEN
         DO 20 I= 1,IW(JHTTR)-LMHLEN,12
            WRITE(LOUTIO,510) I,I+11,(IW(KHTTR+K),K=1,12)
   20    KHTTR = KHTTR + 12
      ENDIF
C
      RETURN
  500 FORMAT (/1X,'+++HCTRGA+++ HTTR  McHcTrigSignal (Mev)')
  510 FORMAT (1X,I3,'-',I3,12I9)
      END
#endif
