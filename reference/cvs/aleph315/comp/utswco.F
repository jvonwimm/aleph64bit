      SUBROUTINE UTSWCO(IL,IWIRE,ZSW,RSW,PHSW)
C-----------------------------------------------------------------------
C! Get sense wire coord (+sag) for fitting
C!
CKEY COMPUTE FIT /INTERNAL
C!
C!    Author          :- J. Sedgbeer
C!    Last Modified   :- J. Sedgbeer   89/09/01
C!    Changed to use ZSW : W.B. Atwood 89/10/4
C!    Input:
C!      IL     /I  : layer number
C!      IWIRE  /I  : wire number                      [1,960]
C!      ZSW    /R  : Z from where ever
C!                   /ITWICC/  ITC sense wire geom.
C!                   /ISWPHI/  ITC sense wire coords.
C!                   /IRESOL/  for sigma z
C!                   ALCONS
C!
C!    Output:
C!      RSW    /R  : Radius of sense wire (cm.)
C!      PHSW   /R  : Phi of sense wire    (radians.)
C!
C!    calls     : none
C!
C!    Libraries required : none
C!
C? Get hit wire coord from wire number
C? calculate wire sag at this Z
C? Correct wire coord. due to sag.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "alcons.h"
#include "itwicc.h"
#include "izfecc.h"
#include "iswphi.h"
#include "iresol.h"
C-----------------------------------------------------------------------
C
C Get sense wire coord. unsagged in ITC frame.
C
      RSW   = RWIRIT(IL)
      PHSW  = PHSWIS(IWIRE)
C
C Calculate the sag at ZSW (assume a parabola)
C
      SAG = SGMXIT*(1.0 - (ZSW/WZMXIT)**2)
C
C Calculate change in RSW and PHSW due to sag
C
      DELR = SAG*SIN(PHSW)
      RSW  = RSW - DELR
      DPHI = SAG*COS(PHSW)/RSW
      PHSW   = PHSW - DPHI
C
C Angles to be in range 0 - 2pi
C
      IF(PHSW.LT.0.0)   PHSW = PHSW + TWOPI
      IF(PHSW.GE.TWOPI) PHSW = PHSW - TWOPI
C
  999 CONTINUE
      END
#endif
