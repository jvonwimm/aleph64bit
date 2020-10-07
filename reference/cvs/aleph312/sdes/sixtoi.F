      SUBROUTINE SIXTOI(RED,PHU,ZED,IRD,IPH,IST,DRA,DP,IOK)
C.---------------------------------------------------------------------
CKEY SCALDES ENCODE ADDRESS / USER
C     B.BLOCH       October 91
C! Address encoding routine from R,PHI,ZED  Radial coordinates to add
C! indices
C   Input :
C          RED ,PHU,ZED  R,PHI,Z coordinates of space point
C   Output:
C          IRD,IPH,IST   corresponding bins in R , Phi , Z
C          DRA           offset[0.,1.] Within bin IRD
C          DP(3)         offset[0.,1.] Within bin IPH in 3 offset planes
C          IOK non 0 means error    -1 wrong in R
C                                   -2 wrong in Z
C                                   -3 wrong in Phi
C                                   -5 falls in gap between halves
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "alcons.h"
#include "sigeco.h"
#include "sinali.h"
#include "sintjj.h"
      PARAMETER ( RDEF = 16. ,YTDEF = 24.5 )
      PARAMETER ( EPS  = 0.001 )
      DIMENSION DP(3)
#include "bmacro.h"
      IOK = -4
C GET module
      IMD = 1
      IF ( ZED.LT.0.) IMD = 2
      X = RED* COS(PHU)
      Y = RED* SIN(PHU)
      Z = ABS(ZED)
#include "siars.h"
C
C Offsets within bin in R and Phi
C
      DRA= (RAD-RMINSI(IMD)-DR -(IRD-1)*RADSTP)/RADSTP
      DO 10 I = 1,3
         DP(I) =   PHI-PHSHFT(I,IMD)
         IF (DP(I).LT.0.) DP(I)= DP(I)+TWOPI
         IF (DP(I).GT.TWOPI) DP(I)= DP(I)-TWOPI
         DP(I) =  (DP(I)-(IPH-1)*PHISTP )/PHISTP
 10   CONTINUE
      IOK = -5
      IF (     X0.LT.GAPXSI(IMD)) GO TO 999
      IOK = 0
 999  RETURN
      END
#endif
