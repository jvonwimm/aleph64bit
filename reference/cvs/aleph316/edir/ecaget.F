       SUBROUTINE ECAGET (TGV , EWTOT )
C----------------------------------------------------------------------
CKEY EDIR HIGHEST CLUSTERS
C! Ecal wire energy of the two most energetic calobjects.
C-
C   Input  : None
C   Output : TGV1(1-2) = Energy of the 2 most energetic cluster
C            TGV1(3-4) = Polar angleof the 2 most energetic cluster
C            TGV1(5)   = Angle between the 2 most energetic cluster
C            TGV1(6-7) = PECO # of the 2 most energetic cluster
C            EWTOT  = Total Ecal wire energy
C-
C   Called by   : SELCAL
C   Calls  : RPECO
C   Input banks : PEWI,PRPW
C-
C                                    Author: S.Dugey  - 910400
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
C --
       COMMON / EWIR / EWIRE ( 36 )
       DIMENSION  TGV(7)
       LOGICAL NODATA
       EXTERNAL NLINK
C --
#include "bmacro.h"
C --
       CALL VZERO(TGV,7)
       ECATOW = 0.
       EWECA  = 0.
       EWECB  = 0.
       EWBAR  = 0.
       NMODI  = 0
       EMAXI = 0.
       EMAXI2= 0.
C --
C   Is the PEWD bank here ??????????
C --
       KPEWI = NLINK('PEWI',0)
       IF ( KPEWI.EQ.0) KPEWI = NLINK('PWEI',0)
       IF ( KPEWI.EQ.0) GOTO 999
C --
C   Loop on all modules ( WIRES energy )
C --
      DO 10 MODULE = 1,36
          I = MODULE
          IF(MODULE.LT.13) EWECA = EWECA + EWIRE(I)
          IF(MODULE.GT.24) EWECB = EWECB + EWIRE(I)
          IF(MODULE.GT.12.AND.MODULE.LT.25) EWBAR = EWBAR + EWIRE(I)
          IF(EWIRE(I).GT.1.0) NMODI = NMODI + 1
  10  CONTINUE
C --
C   TOWERS energy ( loop on PECO - store the 2 most energetic calobjects)
C --
      CALL RPECO( ECATOW , TGV )
C --
C   Loop on PRPW to see if the 2 Calobjects are between several modules
C --
      IF(TGV(1).GT.0.) THEN
        NMO1= 0
        NMO2= 0
        NPRPW = NAMIND ('PRPW')
        KPRPW = IW (NPRPW)
        NPRPW = LROWS (KPRPW)
        IF ( KPRPW .NE . 0) THEN
          DO 20 I = 1,NPRPW
            NPEC = ITABL( KPRPW,I,1)
            IF(NPEC.EQ.TGV(6)) THEN
              NUM = ITABL(KPRPW,I,3)
              IF ( NUM.LT.1.OR.NUM.GT.36) THEN
                IF(IW(6).GT.0) WRITE(IW(6),*) ' ECAGET_MODULE!',NUM ,I
              ENDIF
              IF ( NUM.GT.0.AND.NUM.LT.37) THEN
                IF(EWIRE(NUM).GT.1.) NMO1 = NMO1 + 1
              ENDIF
            ENDIF
            IF(TGV(2).NE.0..AND.NPEC.EQ.TGV(7)) THEN
              NUM = ITABL(KPRPW,I,3)
              IF ( NUM.GT.0.AND.NUM.LT.37) THEN
                IF(EWIRE(NUM).GT.1.) NMO2 = NMO2 + 1
              ENDIF
            ENDIF
 20       CONTINUE
        ENDIF
      ENDIF
      IF(NMO1.GT.1) NMODI = NMODI - NMO1 + 1
      IF(NMO2.GT.1) NMODI = NMODI - NMO2 + 1
C --
C   Highest modules
C --
      IMAXI = 0
      DO I=1,36
        IF(EWIRE(I).GT.EMAXI) THEN
          IMAXI = I
          EMAXI = EWIRE(I)
        ENDIF
      ENDDO
      IMAXI2= 0
      DO 200 I=1,36
        IF(EWIRE(I).GT.EMAXI2.AND.I.NE.IMAXI) THEN
          EMAXI2= EWIRE(I)
        ENDIF
 200  CONTINUE
C --
 999  CONTINUE
C --
       EWTOT = EWECA+ EWECB+ EWBAR
C --
       RETURN
       END
#endif
