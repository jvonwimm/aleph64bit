      SUBROUTINE TCRTRN(ISLOT,R,PHI,Z,RC,PHIC,ZC)
C
C-----------------------------------------------------------------------
C! New transverse drift velocity correction of TPC Coordinates
C! Used for 1997 alignment procedure where transverse drift depends on the
C! slot number (rather than the endplate) and also affects Z.
C! This routine is a modification of TCRTRA and is compatible with
C! either new or old alignment constants.
C!
C!  Author    :   A. Jahn    89/06/23
C!  Modified  :   R. Johnson 90/06/06   To operate stand-alone in
C!                                      the ALEPHLIB.
C!
C!                F.Ranjard  92/02/24   To call TPDVEL
C!                W.Wiedenmann          Use new alignment with calculated   
C!                                      transverse drift velocities.
C!                                      Create TCRTRN from TCRTRA
C!
CKEY TPC,E-FIELD,DISTORTIONS
C!
C!  Input     :
C!                ISLOT/I  : TPC sector number (1-36)
C!                R    /R  : radius of TPC coordinate  [cm]
C!                PHI  /R  : azimuthal angle of TPC coordinate [rad]
C!                Z    /R  : z of TPC coordinate [cm]
C!
C!  Output     :  RC   /R  : corrected radius
C!                PHIC /R  : corrected azimuthal angle
C!                ZC   /R  : corrected z coordinate
C!
C!  Description
C!  ===========
C!  TCRTRN takes the drift-velocity components ux, uy and uz as
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
C
#include "alcons.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "evehjj.h"
#include "talign.h"
#include "tpcdrift.h"
C
      DIMENSION DVX(2),DVY(2),DVZ(2), DVA(3),DVB(3)
      LOGICAL FIRST
      INTEGER AGETDB
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      rc   = r
      phic = phi
      zc   = z

C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTNLO=NAMIND('TNLO')
        NEVEH=NAMIND('EVEH')
        IRLST=0
      ENDIF
C
      iend = iendtp(islot)
      signz = 1.
      If (islot.gt.18) signz = -1.
C
      XMEAS = R*COS(PHI)
      YMEAS = R*SIN(PHI)
C
C++   Get the current run number
C
      KEVEH=IW(NEVEH)
      IF (KEVEH.EQ.0) THEN
        CALL ALTELL('TCRTRN: Cannot find eveh bank to get the'//
     &              ' run number.',0,'RETURN')
        IRUN=4001
      ELSE
        IRUN=IW(KEVEH+JEVERN)
      ENDIF
C
C++   Link to the TPC TNLO bank
C
      IF (IRUN.LE.2000) THEN
        RETURN
      ELSE
C
C........ Check if there is a valid TNLO bank
        IF (IW(NTNLO).GT.0) THEN
C
C..........  Compute driftlength zs
C
             Call Tctsec(islot,r,phi,z,rs,rphis,zs)
C
C........... Correct coordinates
C
             xreal = xmeas - signz*zs*tpc_uxy(1,islot)
             yreal = ymeas - signz*zs*tpc_uxy(2,islot)
C
          Else
             CALL TPDVEL ('POT',DVA,DVB,IER)
             IF (IER.NE.0) Return
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
          Endif
      ENDIF
C
      RC= SQRT( XREAL**2 + YREAL**2)
      PHIC= ATAN2( YREAL,XREAL )
C
      IF (PHIC.LT.0.) THEN
        PHIC = PHIC + TWOPI
      ELSEIF (PHIC.GT.TWOPI) THEN
        PHIC = PHIC - TWOPI
      ENDIF
      IRLST = IRUN
C
      RETURN
      END
#endif
