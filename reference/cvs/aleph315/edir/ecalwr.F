      SUBROUTINE ECALWR(IMODUL,ECAPEA,ECALBE,ECAPEB)
C----------------------------------------------------------------------
CKEY EDIR ECAL WIRE ENERGY
C! ECAL wire energy.
C-
C   Input  : None
C   Output : IMODUL : Number of Ecal modules with energy > 2.5 GeV each
C            ECAPEA : Ecal endcap A wire energy
C            ECALBE : Ecal barrel wire energy
C            ECAPEB : Ecal endcap B wire energy
C-
C   Called by   : ECALSL
C   Calls  : None
C   Input banks : PEWI
C-
C                                        Author: M. Talby September 89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "pewijj.h"
#include "bmacro.h"
C --
      IMODUL = 0
      ECAPEA = 0.
      ECAPEB = 0.
      ECALBE = 0.
C --
      KPEWI=IW(NAMIND('PEWI'))
      IF(KPEWI.EQ.0) KPEWI =IW(NAMIND('PWEI'))
      IF(KPEWI.LE.0) GOTO 999
C --
      NMODUL = LROWS(KPEWI)
C --
C   Loop on Ecal modules above threshlod
C --
      DO 10 N = 1,NMODUL
        MN = ITABL(KPEWI,N,JPEWMN)
        IF(MN.LT.1 .OR. MN.GT.36) GOTO 10
C --
C   Add all planes energy per module
C   Planes with energy =< -100 MeV are not counted
C --
        EMODUL = 0.
        DO 20 NP = JPEWPD,JPEWSS-1
          EPLAN = FLOAT(ITABL(KPEWI,N,NP))*0.000001
          IF(EPLAN.LT.-0.1) GOTO 20
          EMODUL = EMODUL+EPLAN
   20   CONTINUE
        IF(MN.LE.12) ECAPEA = ECAPEA + EMODUL
        IF(MN.GT.12 .AND. MN.LE.24) ECALBE = ECALBE + EMODUL
        IF(MN.GT.24) ECAPEB = ECAPEB + EMODUL
        IF(EMODUL.LT.2.5) GOTO 10
        IMODUL = IMODUL + 1
   10 CONTINUE
C --
  999 RETURN
      END
#endif
