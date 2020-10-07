      FUNCTION CQTPN (ITK)
CKEY PART /USER
C----------------------------------------------------------------------
C! return particle name CQTPN for track ITK
C called from anywhere
C                                                   H.Albrecht 18.10.88
C----------------------------------------------------------------------
#ifndef DOC
C
#include "qcdesh.h"
C
      CHARACTER * (*) CQTPN
      CHARACTER * 12 CQPART
C
C----------------------------------------------------------------------
C
      CQTPN = CQPART (IW(KOQVEC+ITK*KCQVEC+JQVEPA))
      END
#endif
