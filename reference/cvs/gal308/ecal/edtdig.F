*DK edtdig
      SUBROUTINE EDTDIG
C-------------------------------------------------------------------
C      O.CALLOT   27-NOV-85
C! Convert signal to digit
C. - called from ECDIGI                                   this .HLB
C
C------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "ecnamc.h"
#include "edcond.h"
#include "edpara.h"
      EXTERNAL RNDM
#include "bmacro.h"
C ------------------------------------------------------------
      IF(IDETTM .EQ. 0 ) GOTO 980
      NETTM = LROWS(IDETTM )
      LETTM = LCOLS (IDETTM)
      KETTM = IDETTM + LMHLEN
      DO 10 I=1,NETTM
        JPHI = IW(KETTM+1)
        KTET = IW(KETTM+2)
        DO 20 L=1,3
          GAIN = EDTGAI(L) * ( 1. + EDTGER(L) * ( RNDM(L) - .5 ))
          IW(KETTM+2+L) = NINT( FLOAT(IW(KETTM+2+L)) * GAIN )
          IF(EDSAVG.EQ.'YES') RW(KETTM+8+L) = GAIN
   20   CONTINUE
        KETTM = KETTM + LETTM
   10 CONTINUE
  980 RETURN
      END
#endif
