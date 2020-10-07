      INTEGER FUNCTION SIRTOI(RAD,IMD)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE ADDRESS / USER
C     B.BLOCH       February 92
C! Find R bin number from R position in module
C   Input :
C          RAD  Radius (cm)
C          IMD  Module number ( 1 or 2)
C   Output:
C          SIRTOI integer R bin ( 1 to 16)
C                 0 means outside detector or error
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
#include "sigeco.h"
      SIRTOI = 0
C GET RADIUS  bin
      IF ( RAD.LT.RMINSI(IMD)) GO TO 999
      IRD = INT((RAD-RMINSI(IMD))/RADSTP)+1
      IF ( IRD.LE.0 .OR. IRD.GT.NRBNSI) GO TO 999
      SIRTOI = IRD
 999  RETURN
      END
#endif
