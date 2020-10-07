      SUBROUTINE HYTRSF (X,Y,IP,IM)
C --------------------------------------------
CKEY HCALDES HCAL ENDCAP TRANSFORM COORDINATES / USER
C
C! Trasform to plane coordinate system for the end-caps
C!
C!    Author      : G.Zito   85/05/21
C!    modified by : G.Catanesi 87/10/21
C!
C!   -Called by : HCCOIN
C!   -Calls     : none
#ifndef DOC
#include "alcons.h"
#include "hcpara.h"
C ------------------------------------------------------------
C
      IF (IP.EQ.LPECA) THEN
         IF (IM.EQ. 3.OR. IM.EQ.6)                               GOTO 10
      ELSE
         IF (IM.EQ. 1.OR. IM.EQ.4)                               GOTO 10
      ENDIF
                                                                 GOTO 20
C
   10 XI = X
      YI = Y
      R =SQRT(XI*XI+YI*YI)
      XR = XI/R
      IF (ABS(XR).GE.1.) XR =SIGN (1.,XI)
      ANG = PIBY6-ACOS(XR)
      IF(ANG.GT.0.) ANG = 2.*ANG
      IF(ANG.LT.0.)ANG = TWOPI+2.*ANG
      X= COS(ANG)*XI - SIN(ANG)*YI
      Y= SIN(ANG)*XI + COS(ANG)*YI
C
   20 XI = Y
      Y = X
      X = XI
C
      END
C
#endif
