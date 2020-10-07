      FUNCTION CQPART (N)
CKEY PART /USER
C----------------------------------------------------------------------
C! return particle name CQPART for particle code N
C called from anywhere
C                                                   H.Albrecht 18.10.88
C----------------------------------------------------------------------
#ifndef DOC
C
#include "qcdesh.h"
      CHARACTER CHAINT*4, CNAM*12
      CHARACTER * (*) CQPART
C
C----------------------------------------------------------------------
C
      IF (N .NE. 0)  THEN
        CNAM(1:4) = CHAINT (IW(KOQPAR+N*KCQPAR+JQPANA))
        CNAM(5:8) = CHAINT (IW(KOQPAR+N*KCQPAR+JQPANA+1))
        CNAM(9:12) = CHAINT (IW(KOQPAR+N*KCQPAR+JQPANA+2))
      ELSE
        CNAM = ' '
      ENDIF
      CQPART = CNAM
C
      END
#endif
