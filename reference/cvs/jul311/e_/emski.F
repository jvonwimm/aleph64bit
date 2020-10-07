      SUBROUTINE EMSKI
C=======================================================================
C!    Initializes parameters used in mask algorithm
C
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C?
C!======================================================================
#ifndef DOC
#include "emskex.h"
#include "emskpa.h"
      ZHCAL(1) = 354.
      ZHCAL(2) = 415.
      RHCAL(1) = 339.
      RHCAL(2) = 400.
      REMAX = 225.
      ZEMAX = 295.
      PMINC = 0.5
      EMINC = 0.09
      EMIMU = 0.001
C
C-    Fill mask parameters
C     Road width (cm) per stack
C
      XNORM(1) = 4.5
      XNORM(2) = 8.
      XNORM(3) = 12.
      XNORM(4) = 50.
      XNORM(5) = 32.
C
C-    Cuts in units of road width
C
      ZON (1) = 1.
      ZON (2) = 1.
      ZON (3) = 1.
      ZON (4) = 1.
      ZON (5) = 1.
C
C-    Maximum fraction of energy /cell allowed
C-    in each of the 2 regions
C
      EZON (1,1) = 0.3
      EZON (2,1) = 0.6
      EZON (3,1) = 0.6
      EZON (4,1) = 1.2
      EZON (5,1) = 1.2
      EZON (1,2) = 0.2
      EZON (1,1) = 0.1
      EZON (2,1) = 0.16
      EZON (3,1) = 0.1
      EZON (1,2) = 0.08
      EZON (2,2) = 0.1
      EZON (3,2) = 0.1
      EZON (4,2) = 0.2
      EZON (5,2) = 0.2
      EZON (4,1) = 0.6
      EZON (5,1) = 0.6
C
C-    HCAL energy parameters for muon
C
      EMUON = 3.5
      SLOPE = 4.
C
C-    ECAL  energy parameter for muon
C
      ENDPA(1,1) = 3.92
      ENDPA(2,1) = 3.79
      ENDPA(3,1) = 4.13
      ENDPA(1,2) = 1.59
      ENDPA(2,2) = 0.412
      ENDPA(3,2) = 0.316
C
      TILT      = -0.0327
      FUDGE (1) = 1.3
      FUDGE (2) = 1.2
      FUDGE (3) = 1.2
      RETURN
      END
#endif
