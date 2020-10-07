      SUBROUTINE SIPVOI(IR,IP,IM,EVOI)
C------------------------------------------------------------
C!  - For pad indices IR,IP,IM find array of 9 pads in first 5 layers
C  B. Bloch -Devaux  February 93
C  Structure : SUBROUTINE
C           Comdecks references:BCS,BMACRO,SPDAJJ,SCLSJJ
C  Input : IR,IP,IM radius,phi,module index of pad
C  Output : EVOI(45) Array of energies in 9 pads in 5 first layers
C           ( with -Epad if pad is not associated to a cluster )
C  Banks  : input  - SMAP,SPDA
C
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sinamc.h"
#include "spdajj.h"
      DIMENSION EVOI(45)
#include "bmacro.h"
C -------------------------------------------------------------------
C
C
      CALL VZERO(EVOI,45)
      JSMAP = IW(NASMAP)
      JSPDA = IW(NASPDA)
      IF (JSMAP.LE.0) GO TO 999
      IF (JSPDA.LE.0) GO TO 999
C   Loop on 5 planes
      II = 0
      DO 10 IZ = 1,5
        DO 20 JL = IP-1,IP+1
          JN = JL
          IF ( JL.LT.1 ) JN = 32
          IF ( JL.GT.32) JN = 1
          DO 30 KN = IR-1,IR+1
            II = II+1
            IF ( KN.LT.1 ) GO TO 30
            IF ( KN.GT.16) GO TO 30
            CALL SIENCZ(KVOIS,IM,IZ,JN,KN)
            ITOX  = ITABL(JSMAP,KVOIS,1)
            IF (ITOX.EQ.0) GO TO 30
            KLL = KROW(JSPDA,ITOX)
            EVOI(II) = RW(KLL+JSPDEN)
            IF ( IW(KLL+JSPDSC).EQ.0 ) EVOI(II) = -RW(KLL+JSPDEN)
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
  999 RETURN
      END
#endif
