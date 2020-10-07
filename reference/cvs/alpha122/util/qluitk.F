      SUBROUTINE QLUITK (ITK)
CKEY LOCK /USER
C----------------------------------------------------------------------
C! reset lock flag for track ITK.
C
C called from user routines
C                                                   H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C----------------------------------------------------------------------
C
      IW(KOQVEC+ITK*KCQVEC+JQVELK) = 0
      END
#endif
