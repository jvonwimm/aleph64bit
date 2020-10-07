      SUBROUTINE ITSWCO(IL,IWIRE,IZDIG,RSW,PHSW,ZSW,SIGZ)
C-----------------------------------------------------------------------
C! Get sense wire coord (+sag).
C!
CKEY IPREDATA ITC /INTERNAL
C!    Author     :- J. Sedgbeer
C!    Modified   :- J. Sedgbeer   90/01/04 Check use of Z and tidy
C!                                                    Z resolution.
C!    Modified   :- J. Sedgbeer   92/02/07 Use Z non-linearity from
C!                                              /IZNLCC/
C!
C!    Input:
C!      IL     /I  : layer number
C!      IWIRE  /I  : wire number                      [1,960]
C!      IZDIG  /I  : Z TDC value
C!      need commons /IZFECC/  Z front-end parameters
C!                   /IZNLCC/  Z non-linearity params.
C!                   /ITWICC/  ITC sense wire geom.
C!                   /ISWPHI/  ITC sense wire coords.
C!                   /IZRSCC/  for sigma z
C!                   ALCONS
C!
C!    Output:
C!      RSW    /R  : Radius of sense wire (cm.)
C!      PHSW   /R  : Phi of sense wire    (radians.)
C!      ZSW    /R  : Z position           (cm.)
C!      SIGZ   /R  : Sigma Z
C!                   > 0   if all O.K.
C!                  set to large value if IZDIG out of range (ZSW = 0.0)
C!
C!    calls     : none
C!
C!    Libraries required : none
C!
C? Get hit wire coord from wire number
C? calculate z coord from Z scalar value
C? calculate wire sag at this Z
C? Correct wire coord. due to sag.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C I/O commons etc.
#include "alcons.h"
#include "itwicc.h"
#include "izfecc.h"
#include "iznlcc.h"
#include "iswphi.h"
#include "izrscc.h"
C-----------------------------------------------------------------------
C Get sense wire coord. unsagged in ITC frame.
C Set default Z (zero) and error (about 10 times length of chamber).
C
      RSW   = RWIRIT(IL)
      PHSW  = PHSWIS(IWIRE)
      SIGZ  = 2000.
      ZSW   = 0.0
C
C Check Z TDC data O.K.  and check IZDIG value
C
      IF(.NOT.FZCOIZ) GOTO 900
      IF(IZDIG.LE.ITLOIZ(IL)) GOTO 900
C
C Calculate Z coord. and Z error. Z error set to value for track
C finding and fitting (true z resol. only used in special applications).
C If S-bend correction flag, IBNDIZ, is zero then do s-bend correction.
C
      SIGZ0 = ZRTRIZ(IL)
      EFACT = (RSW/RWIRIT(JWIRIT))/EXP8IZ
      TDIFF = (FLOAT(IBN0IZ-IZDIG))*BWIDIZ*EFACT
      Z0    = 0.5*CLGHT*TDIFF
      AA    = TWOPI/CZNLIZ(3)
      IF(IBNDIZ.EQ.0) THEN
        Z0 = Z0 + OFSLIZ(IL)
        ZSW = CZNLIZ(1)*Z0 + CZNLIZ(2)*SIN(AA*Z0)
      ELSE
        ZSW = Z0
      ENDIF
      SIGZ  = SIGZ0
CCC      SIGZ  = SIGZ0*(1. + AA*CZNLIZ(2)*COS(AA*Z0))
C
C Check ZSW in range.  If outside range increase error.
C
      IF(ABS(ZSW).GT.WZMXIT) THEN
         ZSW = WZMXIT*(ABS(ZSW)/ZSW)
         SIGZ  = SIGZ + (ABS(ZSW)-WZMXIT)*3.0
      ENDIF
C
C Calculate the sag at ZSW (assume a parabola)
C
  900 CONTINUE
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
