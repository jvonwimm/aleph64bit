      FUNCTION CORAD94(EN,TH,PH)
C -------------------------------------------------------------
CKEY GAMPACK ECAL
C! Correct ECAL cluster energy
C - Author: M.N Minard          930406
C - Input : EN      : Energy of cluster from 4 towers
C-          TH      : Cluster theta angle (rad)
C-          PH      : Cluster PHI angle (rad)
C - Output: CORAD94 : corrected energy
C -----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "egozjj.h"
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2,LIST*4
C
      DATA DET    / 'EC' /
      DATA LIST   / 'EGOZ' /
      DATA NAEGOZ / 0 /
      DATA IROLD  / 0 /
#include "bmacro.h"
C ----------------------------------------------------------------
       IF ( NAEGOZ.EQ.0) NAEGOZ = NAMIND('EGOZ')
       CORAD94 = 1.
C
C- Get EGCO bank from DB
#include "gtdbbk.h"
       KEGOZ = IW(NAEGOZ)
       IF (KEGOZ.EQ.0) RETURN
C
C-     LOOK FOR CORRECTION FROM EGOZ BANK
C
       NEGOZ = LROWS(KEGOZ)
       COSI = ABS(COS(TH))
       COSI = MIN (COSI,0.999999)
       IROW = 0
       DO IEGOZ = 1,NEGOZ
        IF (COSI.GE.RTABL(KEGOZ,IEGOZ,JEGOTM).AND.
     &      COSI.LT.RTABL(KEGOZ,IEGOZ,JEGOTX)) IROW=IEGOZ
       ENDDO
       GCOR = RTABL(KEGOZ,IROW,JEGOC2) *(1.-
     &        RTABL(KEGOZ,IROW,JEGOC1)/(EN**RTABL(KEGOZ,IROW,JEGOC3)))
       IF(EN.GT.RTABL(KEGOZ,IROW,JEGOC4))THEN
          CORAD94 = 1./GCOR
       ELSE
       ESU = RTABL(KEGOZ,IROW,JEGOC4)
       GCORS= RTABL(KEGOZ,IROW,JEGOC2) *(1.-
     &        RTABL(KEGOZ,IROW,JEGOC1)/(ESU**RTABL(KEGOZ,IROW,JEGOC3)))
       EIN = RTABL(KEGOZ,IROW,JEGOC5)
       GCORI= RTABL(KEGOZ,IROW,JEGOC2) *(1.-
     &        RTABL(KEGOZ,IROW,JEGOC1)/(EIN**RTABL(KEGOZ,IROW,JEGOC3)))
          GCOR1 = (1./(RTABL(KEGOZ,IROW,JEGOC4)-RTABL(KEGOZ,IROW,
     &    JEGOC5)))*(GCORS*(EN-RTABL(KEGOZ,IROW,JEGOC5))
     & + GCORI*(RTABL(KEGOZ,IROW,JEGOC4)-EN))
          CORAD94 = 1./GCOR1
       ENDIF
C
       RETURN
       END
#endif
