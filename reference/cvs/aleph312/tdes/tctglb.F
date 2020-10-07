      SUBROUTINE TCTGLB(JSLOT,RS,RPHIS,ZS,R,RPHI,Z)
C
C-----------------------------------------------------------------------
C! Transform TPC coordinates to global frame,
C! including associated alignment corrections
C!
C!     Author:    R. Johnson 7-2-87
C!     Modified:  R. Johnson 3-8-87
C!     Modified:  W. Wiedenmann 5-3-97
C!                   - correct for transverse drift in tilted 
C!                     coordinate system 
C!                D. Casper 08-05-97
C!                   - disable transverse drift correction if sector number
C!                     is negative (to allow old behavior to be selected)
C!
C!     Input:
C!         - JSLOT    /I     Sector number; if negative, no transverse
C!                           drift correction is applied.
C!         - RS       /R     Radius in sector frame
C!         - RPHIS    /R     R*Phi is sector frame
C!         - ZS       /R     Z in sector frame
C!     Output:
C!         - R        /R     Radius in global frame
C!         - RPHI     /R     R*Phi in global frame
C!         - Z        /R     Z in global frame
C!
C!     Called by TCOOR
C!
C!----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "alcons.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "talign.h"
C
C
      DIMENSION XS(3),XG(3)
C
      ISLOT = ABS(JSLOT)
C
C++   Convert to cartesian coordinates in sector frame
C
      PHIS=RPHIS/RS
      XS(1)=RS*COS(PHIS)
      XS(2)=RS*SIN(PHIS)
      XS(3)=ZS
C
C++   Make a rotation followed by a translation
C
      DO 200 I=1,3
        XG(I)=DSTOGL(I,ISLOT)
        DO 100 J=1,3
          XG(I)=XG(I) + ASTOGL(I,J,ISLOT)*XS(J)
  100   CONTINUE
  200 CONTINUE
C
C++   Correct for transverse drift in tilted coordinate system
C
      IF (JSLOT.GT.0) THEN
        Do i=1,2
         xg(i) = xg(i) - zs*astogl(i,3,islot)
        Enddo
      ENDIF
C
C++   Convert back to cylindrical coordinates
C
      R=SQRT(XG(1)**2+XG(2)**2)
      PHI=ATAN2(XG(2),XG(1))
      Z=XG(3)
C
C++   Place the phi value between 0 and 2pi
C
      IF (PHI.LT.0.) THEN
        RPHI=R*(PHI+TWOPI)
      ELSE
        RPHI=R*PHI
      ENDIF
C
  999 CONTINUE
      RETURN
      END
#endif
