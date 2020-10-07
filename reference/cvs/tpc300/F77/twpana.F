*DK twpana
      SUBROUTINE TWPANA(IT0,NS,JPUL, TIME,PSUM,IGOOD)
C--------------------------------------------------------------------
C! Analyze a TPC wire pulse
C!
C!   Author:     R. Johnson  20-10-86
C!   Modified:   M. Mermikides 7-10-87
C!               D. Cowen      10-5-88 --Use weighted mean as a
C!                                       time estimator as in JULIA.
C!                                       Estimate integrated pulse
C!                                       using pulses above LTRHSH.
C!
C!   Inputs:
C!         - IT0      /I    Time of the first sample in pulse
C!         - NS       /I    Number of samples in pulse (>1)
C!         - JPUL(ns) /I    Pulse height for each sample.  The
C!                          first should be zero.
C!
C!   Output:
C!         - TIME    /R    Best time estimate for the pulse
C!         - PSUM    /R    Pulse area
C!         - IGOOD   /I    = 0 for a pulse of acceptable quality
C!                          = 1 bad pulse (too long or too short)
C!                          = 2 More than one maximum
C!                          = 3 saturated samples
C!
C!   Description
C!   ===========
C!   This routine uses a method of constant fraction discrimination
C!   to determine the best time reference for a pulse.  The time
C!   reference is taken to be the time, based on linear interpolation,
C!   at which the pulse reaches a fraction THRZTW of its maximum
C!   amplitude.
C!
C--------------------------------------------------------------------
C
#include "tpcond.h"
      DIMENSION JPUL(NS)
C
      LOGICAL LUP
C
C++   Find the maximum amplitude of the pulse and its length
C
      IGOOD = 0
      IPHMX=0
      LENP=0
      NSAT = 0
      NPEAK = 0
      PSUM = 0.
      WSUM = 0.
      LUP = .TRUE.
      DO 10 IS=1,NS
         IF (JPUL(IS).GT.IPHMX) IPHMX=JPUL(IS)
         IF (JPUL(IS).GE.LTHRSH) THEN
            LENP = LENP + 1
            PSUM = PSUM + JPUL(IS)
            WSUM = WSUM + (IT0 + IS - 1)*JPUL(IS)
         ENDIF
         IF (JPUL(IS).GE.255) NSAT = NSAT + 1
         IF (IS.GT.1) THEN
C
C  We detect peaks by looking for change in sign of gradient.
C  To avoid effect of small fluctuations, we test for difference
C  between adjacent pulse heights of at least 2 counts.
C
            IF (LUP) THEN
               IF(JPUL(IS).LT.JPUL(IS-1)-2) THEN
                  LUP = .FALSE.
                  NPEAK = NPEAK + 1
               ENDIF
            ELSE
               IF(JPUL(IS).GT.JPUL(IS-1)+2) THEN
                  LUP = .TRUE.
               ENDIF
            ENDIF
         ENDIF
   10 CONTINUE
C
C++   Flag bad pulses
C
      IF (LENP.LT.MINLEN.OR.LENP.GT.NWSMAX) IGOOD = 1
      IF (NPEAK.GT.1) IGOOD = 2
      IF (NSAT. GE.2) IGOOD = 3
      IF (IGOOD.NE.0) GO TO 999
C
C++   Calculate time from weighted mean, as done in JULIA.
C
      TIME = WSUM/PSUM
C
  999 CONTINUE
      RETURN
      END
