*DK sidfrt
      SUBROUTINE SIDFRT
C--------------------------------------------------------------------
C! initialise transverse em-shower distribution in Sical
C  Author: B.Vallage                  5-FEB-1993
C
C    Called by : SIDFPA
C!   Description  : Sical radial shower distribution
C!   ===========
C!                TRIPLE GAUSSIAN Shape with parameters
C!                sigmaa(shower core),sigmab(wings),sigmac(cross talk),
C!                siraab(a over a+b) and sirabc(a+b over a+b+c).
C!
C====================================================================
#ifndef DOC
      INTEGER LAYER
C
#include "iocom.h"
#include "jobcom.h"
#include "siparm.h"
C     sigmaa(shower core)     for the 12 planes
      SIGMAA( 1) =  0.16
      SIGMAA( 2) =  0.18
      SIGMAA( 3) =  0.18
      SIGMAA( 4) =  0.18
      SIGMAA( 5) =  0.20
      SIGMAA( 6) =  0.25
      SIGMAA( 7) =  0.25
      SIGMAA( 8) =  0.30
      SIGMAA( 9) =  0.35
      SIGMAA(10) =  0.41
      SIGMAA(11) =  0.41
      SIGMAA(12) =  0.41
C     sigmab(wings)   for the 12 planes
      SIGMAB( 1) =  0.60
      SIGMAB( 2) =  0.61
      SIGMAB( 3) =  0.59
      SIGMAB( 4) =  0.5925
      SIGMAB( 5) =  0.60
      SIGMAB( 6) =  0.65
      SIGMAB( 7) =  0.65
      SIGMAB( 8) =  0.70
      SIGMAB( 9) =  0.75
      SIGMAB(10) =  0.80
      SIGMAB(11) =  0.85
      SIGMAB(12) =  0.81
C  sigmac(cross talk)   for the 12 planes
      SIGMAC( 1) =  0.95
      SIGMAC( 2) =  1.00
      SIGMAC( 3) =  1.05
      SIGMAC( 4) =  1.15
      SIGMAC( 5) =  1.20
      SIGMAC( 6) =  1.25
      SIGMAC( 7) =  1.30
      SIGMAC( 8) =  1.40
      SIGMAC( 9) =  1.40
      SIGMAC(10) =  1.40
      SIGMAC(11) =  1.40
      SIGMAC(12) =  1.35
C   ratio of gaussians (core / core + wings)  for the 12 planes
      SIRAAB( 1) =  0.80
      SIRAAB( 2) =  0.77693
      SIRAAB( 3) =  0.75456
      SIRAAB( 4) =  0.66242
      SIRAAB( 5) =  0.60077
      SIRAAB( 6) =  0.58030
      SIRAAB( 7) =  0.47305
      SIRAAB( 8) =  0.45
      SIRAAB( 9) =  0.43368
      SIRAAB(10) =  0.40523
      SIRAAB(11) =  0.36658
      SIRAAB(12) =  0.36
Cratio of gaussians (core+wings / core + wings+x-talk) for the 12 planes
      SIRABC( 1) =  0.95
      SIRABC( 2) =  0.87
      SIRABC( 3) =  0.85
      SIRABC( 4) =  0.83
      SIRABC( 5) =  0.75
      SIRABC( 6) =  0.70
      SIRABC( 7) =  0.70
      SIRABC( 8) =  0.63
      SIRABC( 9) =  0.60
      SIRABC(10) =  0.70
      SIRABC(11) =  0.70
      SIRABC(12) =  0.70
C
      IF (IPRIJO(9).GT.0) THEN
         WRITE (LOUTIO,*) ' +++SIDFRT+++ Radial prof parameters'
         WRITE (LOUTIO,*) '  layer  Sigma_a     Sigma_b     Sigma_c',
     &'      A/A+B      A+B/A+B+C  '
         DO 10 LAYER = 1,12
           WRITE(LOUTIO,1001) layer ,sigmaa(layer),sigmab(layer),
     &                     sigmac(layer),siraab(layer),sirabc(layer)
 10      CONTINUE
      ENDIF
C
 1001   FORMAT(5X,I2,5F12.5)
      RETURN
      END
#endif
