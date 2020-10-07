      SUBROUTINE VHERR(IVIEW,PVEC,PH,VERR)
C ---------------------------------------------------------------------
C! Compute the VDET hit error, based on a parameterization of the data
CKEY VGLOB VDET TRACK
C
C      D.Brown - 8-9-94
C
C  INPUT;  IVIEW   = View flag; 1 = W, 2 = U
C          PVEC(3) = track momentum vector at wafer in wafer frame
C          PH      = Pulseheight in normalized units
C OUTPUT;  VERR    = hit error
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  IO variables
C
      INTEGER IVIEW
      REAL PVEC(3),PH,VERR
C
#include "vglbcm.h"
C
      REAL SINFAC, PHFAC
      PARAMETER ( SINFAC = 0.0062 )
      PARAMETER ( PHFAC = 0.0012 )
C
C  Local variables
C
      REAL NORM,SINE
C
C  Compute the sines
C
      IF (IVIEW .EQ. 1) THEN
        SINE = ABS(PVEC(3)) / SQRT(PVEC(1)**2+PVEC(3)**2)
      ELSE
        SINE = ABS(PVEC(2)) / SQRT(PVEC(1)**2+PVEC(2)**2)
      END IF
C
C  Error
C
      VERR = MINERR + PHFAC * ABS(PH-1.0) + SINFAC * SINE**3

      RETURN
      END
#endif
