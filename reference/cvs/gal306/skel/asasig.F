*DK asasig
      SUBROUTINE ASASIG
C ------------------------------------------------------------------
C - F.Ranjard - 860211
C! Analog signal steering routine
C - Compute analog signals with saturation effects in the calorimeters
C - Fill HIT banks for various detectors if required
C
C - modified by : F.Ranjard - 911002
C                 suppress reference to FBEGJO(9)
C                 call SIASIG if SICAl is there
C - modified by : F.Ranjard - 920131
C                 suppress reference to FASGAL and RDST
C
C - Called by   ASPEVE                          from this .HLB
C - Calls       ECASIG, LCASIG, HCASIG          from this .HLB
C               RDMIN , RDMOUT                  from CERNLIB
C -------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
      DATA IFI/0/
C --------------------------------------------------------------------
C
C - Initialize random number if required
C
      IF (IFI.EQ.0) THEN
        IF (IRNDJO(1,3).NE.0) CALL RDMIN (IRNDJO(1,3))
        IFI =1
      ENDIF
      CALL RDMOUT (IRNDJO(1,3))
C
C - loop over some detectors
C
      IF (IDETJO(1).GT.0) CALL VDASIG
      IF (IDETJO(2).GT.0) CALL ITASIG
      IF (IDETJO(3).GT.0) CALL TPASIG
      IF (IDETJO(6).GT.0) CALL SAASIG
      IF (IDETJO(9).GT.0) CALL SIASIG
      IF (IDETJO(4).GT.0) CALL ECASIG
      IF (IDETJO(5).GT.0) CALL LCASIG
      IF (IDETJO(7).GT.0) CALL HCASIG
      IF (IDETJO(8).GT.0) CALL MUASIG
C
      RETURN
      END
#endif
