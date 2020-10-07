*DK sitra3
      SUBROUTINE SITRA3( LAYER , RADIS )
C--------------------------------------------------------------------
C! generate transverse em-shower distribution in Sical
C  Author: J.Rander for Sical         2-SEP-1992
C  Modified B.Vallage                 5-FEB-1993
C
C    Input : LAYER = depth from start of shower (stack)
C    Output: RADIS = distance from the shower axis (in CM)
C    Called by : SISHOW
C!   Description  : Sical radial shower distribution
C!   ===========
C!                TRIPLE GAUSSIAN Shape with parameters
C!                sigmaa(shower core),sigmab(wings),sigmac(cross talk),
C!                siraab(a over a+b) and sirabc(a+b over a+b+c).
C!                Their energy dependence defined in SIDFRT
C!
C====================================================================
#ifndef DOC
      INTEGER LAYER
      REAL RADIS,ALEX,ALEY,SIG,RNDM
      EXTERNAL RNDM
C
#include "siparm.h"
C
      IF(RNDM(RADIS) .LT. SIRABC(LAYER)) THEN
        IF(RNDM(RADIS).LT.SIRAAB(LAYER)) THEN
          SIG = SIGMAA(LAYER)
        ELSE
          SIG = SIGMAB(LAYER)
        ENDIF
      ELSE
        SIG = SIGMAC(LAYER)
      ENDIF
      CALL RANNOR(ALEX,ALEY)
      RADIS = SIG * SQRT(ALEX*ALEX + ALEY*ALEY) /SQRT(2.)
C
      RETURN
      END
#endif
