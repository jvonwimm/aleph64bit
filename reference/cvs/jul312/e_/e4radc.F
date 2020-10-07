      SUBROUTINE E4RADC(COEFF)
C----------------------------------------------------------------------
C!  - ANGLE ENTRE LIGNE DE VOL "PHOTON" ET DRTE EXTRAPLOLEE
C!   Author   :- E. PALLIN
C?
C!======================================================================
#ifndef DOC
#include "e4com0.h"
#include "e4com1.h"
C
C
C   LIGNE DE VOL
C     ENTRY POINT IN THE ECAL X0 Y0 Z0
      X0=XTOW(ITRCL,1,1)
      Y0=XTOW(ITRCL,1,2)
      Z0=XTOW(ITRCL,1,3)
      ALONG=SQRT(X0**2+Y0**2+Z0**2)
      IF(ALONG.EQ.0)THEN
          COEFF=1.
          RETURN
         ENDIF
      AXV=X0/ALONG
      AYV=Y0/ALONG
      AZV=Z0/ALONG
C   COS DIR DTE EXTRAP
      AX=DROITE(ITRCL,1)
      AY=DROITE(ITRCL,2)
      AZ=DROITE(ITRCL,3)
C   DIFFERENCE ANGULAIRE
      COEFF=(AX*AXV+AY*AYV+AZ*AZV)
      IF(COEFF.EQ.0)THEN
          COEFF=1.
          RETURN
         ENDIF
      COEFF=1./(AX*AXV+AY*AYV+AZ*AZV)
      RETURN
      END
#endif
