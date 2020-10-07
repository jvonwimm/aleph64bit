      SUBROUTINE ECPLM(IMOD,ISTA,EWEI)
C----------------------------------------------------------------------
C!  - Ecal correction for wrong plane killed
C!
C! AUTHOR : M.N MINARD 30-09-92
C!
C!     Uses EWKW bank
C!     Input : IMOD  Module number
C!     Input : ISTA  Stack  number
C!     Output: EWEI correction factor
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
      DIMENSION NPLAN (3)
      DATA NPLAN / 10 , 23 , 12 /
      DATA IFIRST / 0 /
#include "bmacro.h"
C
C-    Search if EWKW exist
C
      IF ( IFIRST.EQ.0) THEN
        NAEWKW = NAMIND('EWKW')
        IFIRST =1
      ENDIF
      KEWKW = IW(NAEWKW)
      EWEI = 1.
      IF ( KEWKW.NE.0) THEN
        DO IWR= 1, LROWS(KEWKW)
          IF ( ITABL(KEWKW,IWR,1).EQ.IMOD) THEN
            IPLAN = ITABL(KEWKW,IWR,2)
            IP0 = 0
            DO IP= 1, ISTA
              IF ( IPLAN.LE.NPLAN(IP)+IP0.AND.IPLAN.GT.IP0) THEN
                IF(IP.EQ.ISTA) THEN
                IF ( ITABL(KEWKW,IWR,3).EQ.0) THEN
                  EWEI = FLOAT(NPLAN(IP)-1)/FLOAT(NPLAN(IP))
                ELSE
                  EWEI = FLOAT(NPLAN(IP))/(FLOAT(NPLAN(IP)-1))
                ENDIF
                ENDIF
              ENDIF
              IP0 = IP0 + NPLAN(IP)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
      END
#endif
