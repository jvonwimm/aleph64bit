*DK sitran
      SUBROUTINE SITRAN( LAYER , RADIS )
C--------------------------------------------------------------------
C! generate transverse em-shower distribution in Sical
C  Author: J.Rander for Sical         2-SEP-1992
C
C    Input : LAYER = depth from start of shower (stack)
C    Output: RADIS = distance from the shower axis (in CM)
C    Called by : SISHOW
C!   Description  : Sical radial shower distribution
C!   ===========
C!                DOUBLE GAUSSIAN Shape with parameters
C!                SIGMAA(shower core), SIGMAB(wings), and
C!                SIRAAB(ratio of terms). Their energy
C!                dependence defined in SIDFPA
C====================================================================
#ifndef DOC
      SAVE
#include "siparm.h"
      EXTERNAL RNDM
      SIG=SIGMAA(LAYER)/SQRT(2.)
      IF(RNDM(RADIS).LT.SIRAAB(LAYER))
     &                 SIG=SIGMAB(LAYER)/SQRT(2.)
      CALL RANNOR(ALEX,ALEY)
      XH=ALEX*SIG
      YH=ALEY*SIG
      RADIS=SQRT(XH*XH+YH*YH)
  999 RETURN
      END
#endif
