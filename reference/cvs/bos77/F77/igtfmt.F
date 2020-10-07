      INTEGER FUNCTION IGTFMT(NAMJ)
C --------------------------------------------------
C - F.Ranjard - 900830
C! get bank format index
C   returns the work bank format index for this bank
C - Input   : - NAMJ / I    = bank name-index
C --------------------------------------------------
#ifndef DOC
#include "boscom.h"
C -----------------------------------------------------------------
C     get the format
      IGTFMT=IW(IDFMT+NAMJ-NSYST)
      END
#endif
