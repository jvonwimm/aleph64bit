      SUBROUTINE QLZER (IFLAG)
CKEY LOCK /USER
C----------------------------------------------------------------------
C! reset LOCK for reco (IFL=1) or MC (IFL=2)
C
C called from user routines
C                                                   H.Albrecht 27.11.88
C                                         Modified: E.Blucher  01.08.90
C                                         Modified: E.Blucher  03.02.93
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C----------------------------------------------------------------------
C
      IFL = - IFLAG
      XLREV(IFL) = .FALSE.
      DO 10 I=1,KLOCKM
        KLOCK1(I,IFL) = 0
        KLOCK0(I,IFL) = KLOCK2(I,IFL)
10    CONTINUE
      END
#endif
