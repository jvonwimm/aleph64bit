      SUBROUTINE ECPASR
C-----------------------------------------------------
C! Showers parameters initialisation.
C!
C!  AUTHOR   : J.Badier 16/03/87
C!
C!  BANKS :
C!    INPUT   : NONE
C!    OUTPUT  : NONE
C!    CREATED : NONE
C!
C?
C!======================================================================
#ifndef DOC
#include "ecpash.h"
#include "eratio.h"
C ----- Stack energy cuts for classe definition.
      CUTAEC(1) =.050
      CUTAEC(2) =.070
      CUTAEC(3) =.100
      KLASEC(1) = 1
      KLASEC(2) = 2
      KLASEC(3) = 4
C ----- Decision cut'off
      SEUTEC =.01
      SMT1EC = 9./ 7.
      TOW4EC = 3.
      STRAEC =.8
      SAMBEC =.01
C ----- Energy thresholds.
C ----- Seuil en energie pour analyse longitudinale.
      EMINEC =.1
C ----- Energies de transition.
      ETRAEC =.1
      SEUSEC =.5
C ----- Electron or positron  shower.Energy > ETRAEC
      ASB1EC(1) = 5.1
      ASB2EC(1) = 1.02
      USBEC1(1) = 1.86
      USBEC2(1) = .02
      SGA1EC(1) =.025
      SGA2EC(1) =.085
      SGB1EC(1) =.01
      SGB2EC(1) =.025
C ----- Electron or positron  shower.Energy < ETRAEC
      ASB1EC(4) = 6.03
      ASB2EC(4) = 1.42
      USBEC1(4) = 4.16
      USBEC2(4) = 1.02
      SGA1EC(4) =.025
      SGA2EC(4) =.085
      SGB1EC(4) =.01
      SGB2EC(4) =.025
C ----- Photon shower.
      ASB1EC(2) = 4.15
      ASB2EC(2) = 1.3
      USBEC1(2) = 1.9
      USBEC2(2) = -.02
      SGA1EC(2) =.017
      SGA2EC(2) =.055
      SGB1EC(2) =.016
      SGB2EC(2) = -.00799
C ----- Shower from a pi zero.
      ASB1EC(3) = 4.32
      ASB2EC(3) = .98
      USBEC1(3) = 2.04
      USBEC2(3) = .0
      SGA1EC(3) =.025
      SGA2EC(3) =.085
      SGB1EC(3) =.01
      SGB2EC(3) =.025
C ----- Energies < SEUSEC
      DO 4 I = 1,4
      SGA3EC(I) = SGA1EC(I) + SGA2EC(I) / SEUSEC
      SGB3EC(I) = SGB1EC(I) + SGB2EC(I) / SEUSEC
    4 CONTINUE
C ----- Energy correction in the overlap.
C ----- From Patricia Marotte ( April 1988 ).
      ALAPEC =.33
      TLAPEC =.682
      SLAPEC =.014
C ----- Hadronic weights.
C ----- From HADCAL 88-001, F.Combley et al.( 2.1.1988 )
      CFNTEC(1) = CFNTER(1)
      CFNTEC(2) = CFNTER(2)
      CFNTEC(3) = CFNTER(3)
C
      END
#endif
