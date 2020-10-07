      SUBROUTINE Z0VERI
C-------------------------------------
C     NET = 1 (2) [3] for b (c) [uds]
#include "qcde.h"
      CHARACTER*12 NATFIL
#include "comnnt.h"
#include "qmacro.h"
      ITK = KPDIR('Z0',KMONTE)
      DO 1 I = 1 , KNDAU(ITK)
      NATFIL = CQTPN(KDAU(ITK,I))
      IF (NATFIL.EQ.'uquark'.OR.NATFIL.EQ.'uquark#'.OR.
     >NATFIL.EQ.'dquark'.OR.NATFIL.EQ.'dquark#'.OR.
     >NATFIL.EQ.'squark'.OR.NATFIL.EQ.'squark#')    NET = 3
      IF (NATFIL.EQ.'cquark'.OR.NATFIL.EQ.'cquark#') NET = 2
      IF (NATFIL.EQ.'bquark'.OR.NATFIL.EQ.'bquark#') NET = 1
  1   CONTINUE
      ITQUA = NET
      RETURN
      END

*AF
