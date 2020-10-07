      SUBROUTINE SIZFRI(ZPOS,IST,IMD)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE  / USER
C     B.BLOCH       Marh 93
C! Find Z position  from  z layer and module
C   Input :
C          IST       corresponding Z bin
C          IMD       corresponding Module number
C   Output:
C          ZPOS     Z coordinate  of space point
C   Called by USER program
C.---------------------------------------------------------------------
#ifndef DOC
#include "sigeco.h"
C GET Z   in local coordinates
      ZED = Z0SNSI(IMD)+ ZWIDSI*(IST-1)+ DPOSSI(3,IMD)
C to ARS system
      Z = ZED *(3.-2.*IMD)
      ZPOS = Z
      RETURN
      END
#endif
