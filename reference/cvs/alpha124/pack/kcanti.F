      FUNCTION KCANTI (IPC,IANTI)
CKEY PART /USER
C----------------------------------------------------------------------
C! Return IPC (if IANTI = 0) or anti-particle code (if IANTI .ne. 0)
C!                                                  H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C
      IF (IANTI .NE. 0)  THEN
        KCANTI = IW (KOQPAR + IPC * KCQPAR + JQPAAN)
      ELSE
        KCANTI = IPC
      ENDIF
      END
#endif
