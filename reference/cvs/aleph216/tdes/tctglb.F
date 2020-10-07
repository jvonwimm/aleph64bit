      SUBROUTINE TCTGLB(ISLOT,RS,RPHIS,ZS,R,RPHI,Z)
C
C-----------------------------------------------------------------------
C! Transform TPC coordinates to global frame,
C! including associated alignment corrections
C!
C!     Author:    R. Johnson 7-2-87
C!     Modified:  R. Johnson 3-8-87
C!
C!     Input:
C!         - ISLOT    /I     Sector number
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
      DIMENSION XS(3),XG(3)
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
