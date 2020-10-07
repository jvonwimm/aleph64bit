*DK lcwtri
      SUBROUTINE LCWTRI
C--------------------------------------------------------------
C! Make Wire Trigger bank
C. - J.Dines Hansen & P.Hansen - 861001
C. - Modified P.Hansen - 950202
C. - Fill bank 'LWTR' with the sum of odd and even wire-planes
C. - called by LCTRDI                           from this .HLB
C. - calls     BLIST                            from BOS
C --------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "jobcom.h"
#include "iocom.h"
#include "lccomc.h"
#include "lcnamc.h"
#include "lcparm.h"
#include "bmacro.h"
C--------------------------------------------------------------
C - are there any wire digitalisations
      KWIRE = IW(NALWDI)
      IF(KWIRE .LE. 0)                         GOTO 999
      IF(LROWS(KWIRE) .LE. 0)                  GOTO 999
C
C - Book wire sum digitalisation bank
      LEN = LWTRR * LWTRC + LMHLEN
      CALL ALBOS ('LWTR',0,LEN,KINDX,IGARB)
      IW(KINDX+1) = LWTRC
      IW(KINDX+2) = LWTRR
      CALL BLIST(IW,'E+','LWTR')
      IF (IGARB.EQ.1) KWIRE = IW(NALWDI)
C
C? Sum the wire signals in each module
      DO 200 M = 1,LROWS(KWIRE)
        KRWD = KROW(KWIRE,M)
        MODU = IW(KRWD+1)
        KRWT = KROW(KINDX,MODU)
        IFB  = (MODU-1)/2+1
        DO 100 IPL = 1,LCNWPL
          IWRD = MOD(IPL-1,2)+1
          IW(KRWT+IWRD) = IW(KRWT+IWRD) + IW(KRWD+1+IPL)
  100   CONTINUE
  200 CONTINUE
      IF (FDEBJO .AND. IPRIJO(5).NE.0) THEN
         WRITE(LOUTIO,*) ' +++LCWTRI+++ debug of LWTR bank'
         CALL PRTABL('LWTR',0)
      ENDIF
  999 RETURN
      END
#endif
