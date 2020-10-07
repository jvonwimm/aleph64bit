      REAL FUNCTION SIPLOC(PHI,Z)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE POSITION/ USER
C     B.BLOCH       February 92
C! Find local position inside Phi bin
C   Input :
C          PHI  Azimuth (rad)
C          Z    Z position (cm)
C   Output:
C          SIPLOC Position   within Phi bin (0.,1.)
C                -1.means outside detector or error
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "sigeco.h"
      PARAMETER ( EPS  = 0.001 )
C-------------------------------------------------------
      SIPLOC = -1.
C GET module
      IMD = 1
      IF (Z.LT.0.) IMD = 2
C GET STACK   bin
      IST = INT((ABS(Z)+EPS-Z0SNSI(IMD)-DPOSSI(3,IMD))/ZWIDSI)+1
      IF ( IST.LE.0 .OR. IST.GT.NZBNSI) GO TO 999
C GET PHI     bin
      ITP = MOD(IST-1,3)+1
      PHIT= PHI-PHSHFT(ITP,IMD)
      PHIT = MOD(PHIT,TWOPI)
      IF (PHIT.LT.0.) PHIT= PHIT+TWOPI
      IF (PHIT.GT.TWOPI) PHIT= PHIT-TWOPI
      SIPLOC =(MOD(PHIT,PHISTP))/PHISTP
 999  RETURN
      END
#endif
