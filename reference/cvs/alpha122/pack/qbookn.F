      SUBROUTINE QBOOKN (ID,CHTITL,NVAR,CTAG)
C----------------------------------------------------------------------
CKEY HIST /USER
C! ALPHA shortversion of HBOOKN
C----------------------------------------------------------------------
#ifndef DOC
      LOGICAL HEXIST
      CHARACTER *(*) CHTITL,CTAG(*)
#include "qcdesh.h"
      IF (.NOT. HEXIST (ID))
     +    CALL HBOOKN (ID,CHTITL,NVAR,'ALPHA',10000,CTAG)
      END
#endif
