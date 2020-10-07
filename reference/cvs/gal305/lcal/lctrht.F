*DK lctrht
      SUBROUTINE LCTRHT
C--------------------------------------------------------------
C! Sum up tower hits for triggering
C. - J.Dines Hansen & P.Hansen - 860417
C                                modified by F.Ranjard - 890210
C. - Fills bank LTTR
C. - Called by  LCDIGI                           from this .HLB
C. - Calls      LCSAMP                           from this .HLB
C.              BLIST                            from BOS
C.              VADD                             from CERNLIB
C -----------------------------------------------
#ifndef DOC
      SAVE
      EXTERNAL LCSAMP
#include "iocom.h"
#include "jqcom.h"
#include "lcnamc.h"
#include "lccomc.h"
#include "lcparm.h"
#include "lcmacr.h"
#include "bmacro.h"
C ------------------------------------------------------------
C
C - Book Tower Trigger bank
      LEN = LTTRR * LTTRC + LMHLEN
      CALL ALBOS ('LTTR',0,LEN,KL1TR,IGARB)
      IW(KL1TR+1) = LTTRC
      IW(KL1TR+2) = LTTRR
      CALL BLIST(IW,'E+','LTTR')
C
C - Sum up tower hits in the appropiate trigger amplifier
      KL1TH = IW(NALTHT)
      DO 200 ITOW = 1,LROWS(KL1TH)
         KTH = KROW(KL1TH,ITOW)
         IPAD= IW(KTH+1)
         IAMP= LCSAMP(IPAD)
         IF (IAMP .LE. 0 .OR. IAMP .GT. LTTRR)  GOTO 997
         KTR = KROW(KL1TR,IAMP)
         DO 220 ISTOR = 1,3
            IW(KTR+ISTOR) = IW(KTR+ISTOR) + IW(KTH+ISTOR+1)
  220    CONTINUE
  200 CONTINUE
      GOTO 999
C
 997  WRITE (LOUTIO,'(1X,''+++LCTRHT+++ wrong data in row# '',9I9)')
     &        ITOW,(IW(KTH+M),M=1,LCOLS(KL1TH)),IAMP
C
  999 RETURN
      END
#endif
