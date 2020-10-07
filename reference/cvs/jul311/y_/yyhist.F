      SUBROUTINE YYHIST
C
C----------------------------------------------------------------
C! Fill histograms for main vertex(Mermekides), topology and V0
C!
C!    Author:   M. Mermikides  11 Oct 1987
C!    Modified: M.A. Ciocci,L. Rolandi 4/5/88
C!    Modified: M.Bosman 12/04/89
C?
C?                    Description
C?                    ===========
C?      CALL: YFHIST, fill histograms for main vertex (Mermekides)
C?      CALL: YTHIST, fill histograms topology package
C?      CALL: YVHIST, fill histograms for the V0 package
C?
C----------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
C
C if Mermekides main vertex requested call histogramming routine
      IF(FDETRF(JULYR)) CALL YFHIST
C if topology reconstruction requested call histogramming routine
      IF(FDETRF(JULYT)) CALL YTHIST
C call V0 histogramming routine
      CALL YVHIST
C
      RETURN
      END
#endif
