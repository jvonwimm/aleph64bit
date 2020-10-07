      SUBROUTINE QAINTA
CKEY PACK /INTERNAL
C----------------------------------------------------------------------
C! interactive job ?
C The appropriate CERNLIB routine INTRAC does not exist on IBM (?).
C Set terminal output unit.
C                                                   H.Albrecht 10.11.88
C                                   CRAY flag added E.Blucher  09.09.89
C----------------------------------------------------------------------
#ifndef DOC
C
#include "qcdesh.h"
      LOGICAL INTRAC
C
      XFLIAC = .TRUE.
      XFLIAC = INTRAC (DUMMY)
      IF (.NOT. XFLIAC)  KUPTER = 0
      END
#endif
