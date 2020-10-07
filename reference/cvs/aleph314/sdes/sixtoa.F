      SUBROUTINE SIXTOA(IAD,IOR,XX,YY,ZZ,IOK)
C.---------------------------------------------------------------------
CKEY SCALDES ENCODE ADDRESS / USER
C     B.BLOCH       October 91
C! Address encoding routine  from X,Y,Z coordinates
C   Input :
C          XX ,YY ,ZZ  coordinates of space point
C   Output:
C          IAD   ADDRESS encoded (16 bits) for SIDI
C          IOR   Bin number in triplet (0-2)
C          IOK non 0 means error    -1 wrong in R
C                                   -2 wrong in Z
C                                   -3 wrong in Phi
C                                   -4 wrong in address coding
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
#include "bmacro.h"
      IAD = -1
      IOK = -4
C GET module
      IMD = 1
      IF (ZZ.LT.0.) IMD = 2
      Z = ABS (ZZ)
      X = XX
      Y = YY
#include "siars.h"
C  Then  encode address from elements
      IOK = -4
      CALL SIENCD(IAD,IOR,IMD,IST,IPH,IRD)
      IF ( IAD.EQ.-1) GO TO 999
      IOK = -5
      IF (     X0.LT.GAPXSI(IMD)) GO TO 999
      IOK = 0
 999  RETURN
      END
#endif
