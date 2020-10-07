      SUBROUTINE SIITOR(IMD,IST,IPH,IRD,ZZ,PHI,RAD)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE / USER
C     B.BLOCH       May    1992
C! Transform subcomponents indices into Z,phi,rho coordinates
C   Input :
C          IMD   Module number ( 1-2)
C          IST   Z stack number ( 1-12)
C          IPH   Phi bin number ( 1-32)
C          IRD   Radial bin number ( 1-16)
C   Output:
C          ZZ,PHI,RAD    corresponding z, phi ( degrees), rho
C          RAD = -1.     means error
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "alcons.h"
#include "sigeco.h"
#include "sinali.h"
#include "sintjj.h"
      PARAMETER ( RDEF = 16. ,YTDEF = 24.5 )
#include "bmacro.h"
      RAD =-1.
      IOR = MOD(IST-1,3)
#include "siloca.h"
C transform to desired coordinates
      RAD = SQRT(X*X + Y* Y )
      PHI = ATG (Y,X)
      PHI = PHI*RADEG
      ZZ = Z
 998  RETURN
      END
#endif
