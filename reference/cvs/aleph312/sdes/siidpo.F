      SUBROUTINE SIIDPO(IMD,IST,IPH,IRD,ZZ,PHI,RAD)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE / USER
C     B.BLOCH     December 1993
C! Transform subcomponents indices into Z,phi,rho coordinates
C! without fine alignment = ideal positions ( to be used within DALI)
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
#include "bmacro.h"
      RAD =-1.
      IOR = MOD(IST-1,3)
C GET RADIUS
      RAD = RMINSI(IMD)+ RADSTP*(IRD-1)+0.5*RADSTP
C GET PHI
      ITP = IOR +1
      PHI = PHISTP*(IPH-1) + PHSHFT(ITP,IMD)+0.5*PHISTP
C GET Z
      ZED = Z0SNSI(IMD)+ ZWIDSI*(IST-1)
      IF ( IMD.EQ.2 ) ZED = -ZED
C
      PHI = PHI*RADEG
      ZZ = ZED
 998  RETURN
      END
#endif
