      SUBROUTINE SILTOX(RLO,PLO,IST,IMD,RCL,PCL)
C.---------------------------------------------------------------------
CKEY SCALDES ENCODE ADDRESS / USER
C     B.BLOCH       October 91
C! Geometrical transformation routine from local R,PHI  cylindrical
C! coordinates to ARS cylindrical coordinates
C   Input :
C          RLO ,PLO   R,PHI coordinates of space point in local system
C          IST ,IMD   plane and module indices
C   Output:
C          RCL,PCL       corresponding coordinates in ARS sytem
C
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "alcons.h"
#include "sigeco.h"
#include "sinali.h"
#include "sintjj.h"
      PARAMETER ( RDEF = 16. ,YTDEF = 24.5 )
#include "bmacro.h"
C! transform to ARS system from local system
      IROW = ISINUM(IST,IMD)
      ITP = MOD(IST-1,3)+1
      IPH =(PLO-PHSHFT(ITP,IMD))/PHISTP +1
      IF ( IPH.GT.32 ) IPH = IPH -32
      IXTAL = (IPH-1)/2 +1
      JSINT = IW(NASINT)
      IF ( JSINT.LE.0 ) GO TO 998
      ISIDE = 1
      IF ( IPH.GE.9 .AND. IPH.LE.24 ) ISIDE = -1
C GET RADIUS and phi
      DR = RTABL(JSINT,IROW,JSINXR+IXTAL-1) - RDEF
      DXT= RTABL(JSINT,IROW,JSINTP)
      DYT= RTABL(JSINT,IROW,JSINTP+1) - YTDEF
      RAD = RLO + DR
      PHI = PLO
C transform into ARS cartesian system
      X = RAD * COS(PHI) - DXT + DPOSSI(1,IMD) + ISIDE * GAPXSI(IMD)
      Y = RAD * SIN(PHI) - DYT + DPOSSI(2,IMD)
      RCL = SQRT(X*X + Y*Y)
      PCL = ATG(Y,X)
 998  return
      END
#endif
