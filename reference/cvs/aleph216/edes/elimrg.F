      SUBROUTINE ELIMRG (XRG,XRWFR,XRWLS)
C.-----------------------------------------------------------------
CKEY ECALDES ROW REGION LIMIT /USER
C     H.Videau      Creation 03/03/89  Modification 14/03/90
C! Get rows limiting region
C   Input :
C           XRG   extended region number (1->7)
C   Output: XRWINF first row of the region XRG
C           XRWSUP last  row of the region XRG
C   Called by USER
C Should be used at initialisation only..
C.-----------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
C  Input
          INTEGER XRG
C  Output
          INTEGER XRWFR,XRWLS
C  Locales
          INTEGER I
C
       XRWFR=EXRGFR(XRG)
       XRWLS=EXRGLS(XRG)
      END
#endif
