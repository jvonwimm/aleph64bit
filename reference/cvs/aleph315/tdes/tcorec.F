      SUBROUTINE TCOREC(RAD,PHI,Z)
C
C-----------------------------------------------------------------------
C! Correct TPC Coordinates for Imperfections of the Drift Field
C! ------------------------------------------------------------
C!
C!  Author    :   M. Schmelling   88/08/30
C!
C!  Input     :
C!                RAD /R  : radius of TPC coordinate  [cm]
C!                PHI /R  : azimuthal angle of TPC coordinate [rad]
C!                Z   /R  : z of TPC coordinate [cm]  (not changed)
C!
C!  Output     :  RAD /R  : corrected radius
C!                PHI /R  : corrected azimuthal angle
C!
C!  Called by TCOOR
C!  External references   : FINT  Cernlib E104
C!
C!  Description
C!  ===========
C!  TCOREC applies a first order correction to the transverse
C!  components of individual TPC coordinates. The coordinates have
C!  to be given in the overall ALEPH reference frame.
C!
C!  To apply the correction the user has to specify the option
C!  'TCOR' on the TOPT steering card.
C!
C!  --> Currently only the imperfections of the magnetic field <--
C!  --> are taken into account.                                <--
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "alcons.h"
#include "bcs.h"
#include "tfcorr.h"
C
      DIMENSION    CVEC(3)
C
C--      INTERPOLATION TO ESTIMATE COORDINATE DISPLACEMENTS
C----------------------------------------------------------
C
      CVEC(1) = RAD
      CVEC(2) = PHI
      CVEC(3) = ABS(Z)
C
 1001 CONTINUE
      IF(CVEC(2).LT.   0.) THEN
         CVEC(2) = CVEC(2) + TWOPI
         GOTO 1001
      ENDIF
      IF(CVEC(2).GT.TWOPI) THEN
         CVEC(2) = CVEC(2) - TWOPI
         GOTO 1001
      ENDIF
C
      IF(Z.GE.0.) THEN
         DRAD = FINT(3,CVEC,NAFCOR,AFCORR,FSPACE(INDCOR(1)))
         DPHI = FINT(3,CVEC,NAFCOR,AFCORR,FSPACE(INDCOR(3)))
                  ELSE
         DRAD = FINT(3,CVEC,NAFCOR,AFCORR,FSPACE(INDCOR(2)))
         DPHI = FINT(3,CVEC,NAFCOR,AFCORR,FSPACE(INDCOR(4)))
      ENDIF
C
C
C--      COORDINATE CORRECTIONS
C------------------------------
C
      RAD = RAD - DRAD
      PHI = PHI - DPHI
      IF (PHI.LT.0.) THEN
        PHI = PHI + TWOPI
      ELSEIF (PHI.GT.TWOPI) THEN
        PHI = PHI - TWOPI
      ENDIF
C
      RETURN
      END
#endif
