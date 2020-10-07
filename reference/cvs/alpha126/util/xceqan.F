      FUNCTION XCEQAN (ITK,IPC,IANTI)
CKEY PART /USER
C----------------------------------------------------------------------
C! Particle ITK = IPC or its antiparticle ?
C                                                   H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      LOGICAL XCEQAN
C----------------------------------------------------------------------
      IF (IANTI .EQ. 0)  THEN
        XCEQAN = IW(KOQVEC+ITK*KCQVEC+JQVEPA) .EQ. IPC
      ELSE
        XCEQAN = IW(KOQVEC+ITK*KCQVEC+JQVEPA) .EQ.
     &           IW(KOQPAR+IPC*KCQPAR+JQPAAN)
      ENDIF
      END
#endif
