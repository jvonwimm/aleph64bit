      FUNCTION CORADOC(EN,TH,PH)
C -------------------------------------------------------------
CKEY GAMPACK ECAL
C! Correct ECAL cluster energy
C - Author: M.N Minard          930406
C - Input : EN      : Energy of cluster from 4 towers
C-          TH      : Cluster theta angle (rad)
C-          PH      : Cluster PHI angle (rad)
C - Output: CORADOC : corrected energy
C -----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "egcojj.h"
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2,LIST*4
C
      DATA DET    / 'EC' /
      DATA LIST   / 'EGCO' /
      DATA NAEGCO / 0 /
      DATA IROLD  / 0 /
#include "bmacro.h"
C ----------------------------------------------------------------
       IF ( NAEGCO.EQ.0) NAEGCO = NAMIND('EGCO')
       CORADOC = 1.
C
C- Get EGCO bank from DB
#include "gtdbbk.h"
       KEGCO = IW(NAEGCO)
       IF (KEGCO.EQ.0) RETURN
C
C-     Look for correction from EGCO bank
C
       NEGCO = LROWS(KEGCO)
       COSI = ABS(COS(TH))
       COSI = MIN (COSI,0.999999)
       IROW = 0
       DO IEGCO = 1,NEGCO
        IF (COSI.GE.RTABL(KEGCO,IEGCO,JEGCTM).AND.
     &      COSI.LT.RTABL(KEGCO,IEGCO,JEGCTX)) IROW=IEGCO
       ENDDO
       GCOR = RTABL(KEGCO,IROW,JEGCC2) -
     &        RTABL(KEGCO,IROW,JEGCC1)/SQRT(EN)
       IF(EN.LT.RTABL(KEGCO,IROW,JEGCC3))THEN
       GCOR = RTABL(KEGCO,IROW,JEGCC2)+RTABL(KEGCO,IROW,JEGCC1)
     &        *(EN-RTABL(KEGCO,IROW,JEGCC3))/(RTABL(KEGCO,IROW
     &        ,JEGCC4))
       ENDIF
C
       CORADOC = 1./GCOR
       END
#endif
