      SUBROUTINE QEDIRUPD
CKEY FILL LEPTONS/INTERNAL
C ---------------------------------------------------------------------
C! Set Lepton Tagging bits 27 and 28 for event directory
C  Author M-N Minard   28-Nov-1994
C     Bit 27 = Electron selection (Rt>-3, -3<Rl<3))
C     Bit 28 = Muon selection ( QMUIDO flag 13 or 14 )
C  Called from QFLEPT
C ---------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "revhjj.h"
      LOGICAL FUNCTION BTEST
      INTEGER  ISCLAS(32)
#include "qmacro.h"
C ---------------------------------------------------------------------
      KCLASW = 0
      KREVH = IW(NAREVH)
      CALL VZERO(ISCLAS,32)
      IF ( KREVH.NE.0) THEN
C
C-      Decode current event directory flag
C
        KCLASW = IW(KREVH+LMHLEN+JREVEC)
        DO IL = 1,32
          IF(BTEST(KCLASW,IL-1))ISCLAS(IL) = 1
        ENDDO
C
C-      Look to lepton bank content
C
        KPDLT = IW(NAPDLT)
        IF ( KPDLT.GT.0) THEN
          NPDLT = LROWS(KPDLT)
          NLMU = 0
          NLEL = 0
          DO IK = 1,NPDLT
            ITYP = ITABL(KPDLT,IK,JPDLPA)
            IF (MOD(ITYP,10).EQ.2.OR.MOD(ITYP,10).EQ.3) NLEL = NLEL +1
            IF (MOD(ITYP,10).EQ.5.OR.MOD(ITYP,10).EQ.6) NLMU = NLMU+1
          ENDDO
          IF ( NLEL.GT.0) ISCLAS(27) = 1
          IF ( NLMU.GT.0) ISCLAS(28) = 1
        ENDIF
        KCLASW = 0
        DO IL = 1,32
          IF (ISCLAS(IL).EQ.1)   KCLASW = IOR(KCLASW,ISHFT(1,(IL-1)))
        ENDDO
C
C-     Update REVH Bank
C
        IW(KREVH+LMHLEN+JREVEC) = KCLASW
      ENDIF
      RETURN
      END
#endif
