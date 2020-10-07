      REAL FUNCTION HCATOW (ITHETA, IPHI)
C********************************************************************
C! Hcal towers calibration (Intermodule and time dependence)         *
C!                                                                   *
C!    Authors : R.Tenchini                                           *
C!              S.Wasserbaech                                        *
C!                  10.3.90                                          *
C!                                                                   *
C!    Mod     : A.Messineo                                           *
C!                  19.7.95                                          *
C!                                                                   *
C!    Input:                                                         *
C!        ITHETA  (I)     theta index of tower                       *
C!        IPHI    (I)     phi index of tower                         *
C!                                                                   *
C!    Output:                                                        *
C!        HCATOW  (R)     correction factor                          *
C!                                                                   *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "htowca.h"
      LOGICAL BUNCH_OK
#include "bmacro.h"
c.....
C select the wagon number inside a bunch
C first we look if the run have 1 o 4 wagon
C we use the rqbunc routine
c.....
      call RQBUNC(IBUN,INBU,NWAG,IQUA)
c.....
c check on rqbunc
c.....
      BUNCH_OK=NWAG.GT.1.AND.IBUN.GT.0.AND.IBUN.LE.NWAG.AND.IQUA.EQ.3
      IF ( (NWAG.LT.1).OR.(NWAG.GT.4) ) NWAG=1
      IF (.NOT.BUNCH_OK) NWAG = 1
      IF(NWAG.EQ.1) THEN
        CT0DABW = CT0DAB
        CT0DAEW = CT0DAE
      ELSE
       DO I = 1,4
        IF(IBUN.EQ.I) THEN
         CT0DABW = CT0DWB(I) * CT0DAB
         CT0DAEW = CT0DWE(I) * CT0DAE
        ENDIF
       ENDDO
      ENDIF
c...
C     Is this tower in the barrel or endcap?
C     (Hits in the overlap region are assigned to the barrel.)
      CALL HFNDMD(ITHETA,IPHI,1,ISUBC,MODSUB,IOV)
      IF (ISUBC.EQ.2) THEN
C       It's in the barrel.
        HCATOW = CT0MCB/(CT0DABW*CALBA(MODSUB)*CALTBA)
      ELSE
C       It's in the endcaps.
        IF(ISUBC.EQ.3) MODSUB=MODSUB+6
        HCATOW = CT0MCE/(CT0DAEW*CALEC(MODSUB)*CALTEC)
      ENDIF
      RETURN
      END
#endif
