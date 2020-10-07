      SUBROUTINE TCTSEC(ISLOT,RG,PHIG,ZG,RS,RPHIS,ZS)
C
C-----------------------------------------------------------------------
C! Transform coordinates to TPC sector frame,
C! including associated alignment corrections
C!
C!     Author:    R. Johnson 20-3-88
C!     Modified:  R. Johnson 15-4-88
C!
C!     Input:
C!         - ISLOT    /I     Sector number
C!         - RG       /R     Radius in global frame
C!         - PHIG     /R     Phi in global frame
C!         - ZG       /R     Z in global frame
C!     Output:
C!         - RS       /R     Radius in sector frame
C!         - RPHIS    /R     R*Phi in sector frame
C!         - ZS       /R     Z in sector frame
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
      DIMENSION X(3),XS(3)
C
C++   Convert to cartesion coordinates
C
      X(1)=RG*COS(PHIG)
      X(2)=RG*SIN(PHIG)
      X(3)=ZG
C
C++   Correct for transverse drift velocity; unfortunately we need the
C++   drift length to do this...make an approximation.  The worst case
C++   z displacement of a sector due to rotation observed in the data is
C++   50cm * 1mr = 500um, which is much less than the typical drift length.
C++   Any imprecision is then a second order effect, since it will be a
C++   small error in the drift length multiplied by a small angle.
C
      XS(3) = DGLTOS(3,ISLOT)
      DO J = 1,3
        XS(3) = XS(3) + AGLTOS(3,J,ISLOT)*X(J)
      ENDDO
      X(1) = X(1) + XS(3) * ASTOGL(1,3,ISLOT)
      X(2) = X(2) + XS(3) * ASTOGL(2,3,ISLOT)
C
C++   Make a rotation followed by a translation
C
      DO 200 I=1,3
        XS(I)=DGLTOS(I,ISLOT)
        DO 100 J=1,3
          XS(I)=XS(I) + AGLTOS(I,J,ISLOT)*X(J)
  100   CONTINUE
  200 CONTINUE
C
C++   Convert back to cylindrical coordinates.  The angle phi
C++   should be between -pi and pi radians.
C
      RS=SQRT(XS(1)**2+XS(2)**2)
      RPHIS=RS*ATAN2(XS(2),XS(1))
      ZS=XS(3)
C
  999 CONTINUE
      RETURN
      END
#endif
