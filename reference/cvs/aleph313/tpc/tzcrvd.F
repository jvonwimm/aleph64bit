      SUBROUTINE TZCRVD(IEND,R,Z,ZC)
C
C-----------------------------------------------------------------------
C! Correct TPC Coordinates for whatever causes Z residuals in the VDET
C!
C!  Author    :   F. Sefkow  91/09/04
C!
CKEY TPC Z-CORRECTION VDET
C!
C!  Input     :
C!                IEND /I  : side of the TPC (1=A, 2=B)
C!                R    /R  : radius of TPC coordinate  [cm]
C!                Z    /R  : z of TPC coordinate [cm]
C!
C!  Output     :  ZC   /R  : corrected Z coordinate
C!
C!  Description
C!  ===========
C!  Studied:  Z hits in 2 VDET layers (Del R, Del Z apart)
C!  Observed:  Res = alpha * Del Z + beta * Del R
C!             where Res = Z(hit) - Z(track) and
C!             Z(track) calculated from other hit
C!             and track angle lambda as measured by TPC
C!  Possible Interpretation:  alpha: fractional change in v_z(drift)
C!                            beta: bow angle of endplate
C!  Correction:  Z(drift) -> Z(drift) * (1 + alpha)
C!               |lambda| -> |lambda| + beta
C!
C!  Note:  The alpha correction induces a z0 shift
C!         by Del Z0 = - alpha * (+/- ZTPCMX) for tracks in side A/B
C!  Note:  the TPC geometry must be initialized before calling this
C!         routine.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "evehjj.h"
#include "bcs.h"
#include "tzcvjj.h"
#include "tpgpar.h"
#include "tpgeom.h"
C
C++   Correction coefficients for sides A (1) and B (2)
C
      REAL ZCALFA(2), ZCBETA(2)
      LOGICAL FIRST
      INTEGER AGETDB
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
C
      ZC = Z
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTZCV=NAMIND('TZCV')
        NEVEH=NAMIND('EVEH')
        IRLST=0
      ENDIF
C
C++   Get the current run number
C
      KEVEH=IW(NEVEH)
      IF (KEVEH.EQ.0) THEN
        CALL ALTELL('TZCVDT: cannot find EVEH bank to get the'//
     &              ' run number.',0,'RETURN')
        IRUN=4001
      ELSE
        IRUN=IW(KEVEH+JEVERN)
      ENDIF
C
C++   Link to the TPC z correction bank
C
      IF (IRUN.LE.2000) THEN
        RETURN
      ELSE
        IF (IRLST.NE.IRUN) THEN
          IRET=AGETDB('TZCV',IRUN)
          IF (IRET.EQ.0) THEN
            CALL ALTELL(
     &       'Bank TZCV is missing from the database.',0,'RETURN')
          ENDIF
          KTZCV=IW(NTZCV)
          IF (KTZCV.NE.0) THEN
            ZCALFA(1) = RTABL (KTZCV,1,JTZCAA)
            ZCALFA(2) = RTABL (KTZCV,2,JTZCAA)
            ZCBETA(1) = RTABL (KTZCV,1,JTZCBA)
            ZCBETA(2) = RTABL (KTZCV,2,JTZCBA)
          ELSE
            ZCALFA(1) = 0.
            ZCALFA(2) = 0.
            ZCBETA(1) = 0.
            ZCBETA(2) = 0.
          ENDIF
          IRLST=IRUN
        ENDIF
      ENDIF
C
C++   Calculate drift lenghth ZDRFT ( > 0 always)
C
      IF (IEND.EQ.1) THEN
        ZDRFT=ZTPCMX-Z
      ELSE
        ZDRFT=ZTPCMX+Z
      ENDIF
C
C++   Correct for possibly bowed TPC endplate
C
      ZDCOR = ZDRFT - ZCBETA (IEND) * R
C
C++   Correct for drift velocity error
C
      ZDCOR = ZDCOR * (1. + ZCALFA (IEND))
C
      IF (IEND.EQ.1) THEN
        ZC = ZTPCMX-ZDCOR
      ELSE
        ZC = ZDCOR-ZTPCMX
      ENDIF
C
      RETURN
      END
#endif
