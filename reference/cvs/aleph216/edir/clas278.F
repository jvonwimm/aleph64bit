      SUBROUTINE CLAS278 (KLASW)
C ---------------------------------------------------------------------
C! Set Lepton Tagging bits 27 and 28 in REVH class-word
C - M.N Minard   28-Nov-1994
CKEY EDIR
C - Output : KLASW / I = updated REVH class word
C     Bit 27 = Electron selection (Rt>-3, -3<Rl<3))
C     Bit 28 = Muon selection ( QMUIDO flag 13 or 14 )
C  Called from QFLEPT
C ---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "revhjj.h"
#include "pdltjj.h"
      INTEGER KLASW
      DATA NAREVH,NAPDLT /2*0/
#include "bmacro.h"
C ---------------------------------------------------------------------
      IF (NAREVH.EQ.0) THEN
         NAREVH = NAMIND('REVH')
         NAPDLT = NAMIND('PDLT')
      ENDIF
C
      KREVH = IW(NAREVH)
      IF ( KREVH.NE.0) THEN
        IF (LCOLS(KREVH).GE.JREVEC) THEN
C
C - get existing class word
          KLASW = ITABL(KREVH,1,JREVEC)
C
C - Look to lepton bank content
          KPDLT = IW(NAPDLT)
          IF ( KPDLT.GT.0) THEN
            NLMU = 0
            NLEL = 0
            DO IK = 1,LROWS(KPDLT)
              ITYP = ITABL(KPDLT,IK,JPDLPA)
              IF (MOD(ITYP,10).EQ.2.OR.MOD(ITYP,10).EQ.3) NLEL = NLEL+1
              IF (MOD(ITYP,10).EQ.5.OR.MOD(ITYP,10).EQ.6) NLMU = NLMU+1
            ENDDO
            IF ( NLEL.GT.0) KLASW = IBSET (KLASW,27)
            IF ( NLMU.GT.0) KLASW = IBSET (KLASW,28)
          ENDIF
C
C - Update REVH Bank
          IW(KREVH+LMHLEN+JREVEC) = KLASW
        ENDIF
      ENDIF
      END
#endif
