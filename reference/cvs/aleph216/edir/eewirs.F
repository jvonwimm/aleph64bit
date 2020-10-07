      LOGICAL FUNCTION EEWIRS (IDUMMY)
C----------------------------------------------------------------------
CKEY EDIR CLASS26
C!  - select bhabha events for calibration studies
C!
C!   Author   :- E. Lancon             15-APR-1992
C!
C!   Libraries required:
C!
C!   Description : Events are flagged if two modules have ECAL wire
C!   ===========   energies (EWHE) above 30 GeV.
C!
C!   Modified :- E. Lancon              9-JUL-1993
C!                 Use uncalibrated wire energy
C?
C!======================================================================
#ifndef DOC
#include "ewhejj.h"
#include "echejj.h"
#include "bcs.h"
C------------------------------------ Energy Cut 30 GeV = 30000000 KeV
      DATA ECUT / 30000000. /
C
#include "bmacro.h"
C
      EEWIRS = .FALSE.
      NAEWHE = NAMIND('EWHE')
      KEWHE  = IW(NAEWHE)
C
C?   Get Calib. coeff. applyed  on those data
C
      NAECHE = NAMIND('ECHE')
      KECHE  = IW(NAECHE)
      NECHE  = 0
      IF (KECHE.GT.0) NECHE = LROWS(KECHE)
C
      IC = 0
      IF ( KEWHE.GT.0 ) THEN
        NEWHE = LROWS (KEWHE)
        IF ( NEWHE.EQ.36 ) THEN
          DO IEWHE =  1, NEWHE
            IENE = ITABL (KEWHE, IEWHE, JEWHSD)
            IMOD = ITABL (KEWHE, IEWHE, JEWHNU)
            IF (  IMOD.LE.NECHE .AND. IMOD.GT.0 ) THEN
              COEF = RTABL(KECHE, IMOD, JECHCW)
            ELSE
              COEF = 1.
            ENDIF
            ENE = FLOAT(IENE) / COEF
            IF ( ENE.GE.ECUT ) THEN
              IC = IC  + 1
            ELSE
            ENDIF
          ENDDO
        ELSE
        ENDIF
      ELSE
      ENDIF
      IF ( IC.GE.2 ) EEWIRS = .TRUE.
  999 RETURN
      END
#endif
