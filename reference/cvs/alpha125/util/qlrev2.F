      SUBROUTINE QLREV2 (IFLAG)
CKEY LOCK /USER
C----------------------------------------------------------------------
C! locked tracks -> unlocked tracks and vice versa
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
      DO 10 I=1,KLOCKM
        KLOCK2(I,IFL) = NOT (KLOCK2(I,IFL))
        KLOCK0(I,IFL) = IOR (KLOCK1(I,IFL), KLOCK2(I,IFL))
10    CONTINUE
      XLREV2(IFL) = .NOT. XLREV2(IFL)
      END
#endif
