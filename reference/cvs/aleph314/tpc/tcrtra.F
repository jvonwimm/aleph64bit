      SUBROUTINE TCRTRA(IEND,R,PHI,Z,RC,PHIC)
C
C-----------------------------------------------------------------------
C! Correct TPC Coordinates for Transverse Drift Velocity Components
C!
C!  Author    :   A. Jahn    89/06/23
C!  Modified  :   R. Johnson 90/06/06   To operate stand-alone in
C!                                      the ALEPHLIB.
C!
C!                F.Ranjard  92/02/24   To call TPDVEL
CKEY TPC,E-FIELD,DISTORTIONS
C!
C!  Input     :
C!                IEND /I  : side of the TPC (1=A, 2=B)
C!                R    /R  : radius of TPC coordinate  [cm]
C!                PHI  /R  : azimuthal angle of TPC coordinate [rad]
C!                Z    /R  : z of TPC coordinate [cm]  (not changed)
C!
C!  Output     :  RC   /R  : corrected radius
C!                PHIC /R  : corrected azimuthal angle
C!
C!  Description
C!  ===========
C!  TCRTRA takes the drift-velocity components ux, uy and uz as
C!  measured by the laser-calibration system and evaluates the
C!  true coordinates via:
C!            x_real = x_meas + zdrft*(ux/uz)
C!            y_real = y_meas + zdrft*(uy/uz)
C!  where zdrft is the drift length.
C!
C!  Note:  the TPC geometry must be initialized before calling this
C!         routine.
C!         TPDVEL must have been called with the right option either
C!         by JULIA or AUNPCK.
C!         It is always called by AUNPCK at beginning of run with
C!         option 'POT'.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "bcs.h"
#include "alcons.h"
#include "tpgpar.h"
#include "tpgeom.h"
C
      DIMENSION DVX(2),DVY(2),DVZ(2), DVA(3),DVB(3)
C
#include "bmacro.h"
C
      XMEAS = R*COS(PHI)
      YMEAS = R*SIN(PHI)
C
      CALL TPDVEL ('POT',DVA,DVB,IER)
      IF (IER.NE.0) GOTO 998
      IF (IEND.EQ.1) THEN
         DVX(1) = DVA(1)
         DVY(1) = DVA(2)
         DVZ(1) = DVA(3)
         ZDRFT=ZTPCMX-Z
      ELSE
         DVX(2) = DVB(1)
         DVY(2) = DVB(2)
         DVZ(2) = DVB(3)
         ZDRFT=ZTPCMX+Z
      ENDIF
C
      DX = ZDRFT*DVX(IEND)/DVZ(IEND)
      DY = ZDRFT*DVY(IEND)/DVZ(IEND)
C
      XREAL = XMEAS - DX
      YREAL = YMEAS - DY
C
      RC= SQRT( XREAL**2 + YREAL**2)
      PHIC= ATAN2( YREAL,XREAL )
C
      IF (PHIC.LT.0.) THEN
        PHIC = PHIC + TWOPI
      ELSEIF (PHIC.GT.TWOPI) THEN
        PHIC = PHIC - TWOPI
      ENDIF
C
      RETURN
C - drift velocity not found, return without corrections
 998  RC = R
      PHIC = PHI
      END
#endif
