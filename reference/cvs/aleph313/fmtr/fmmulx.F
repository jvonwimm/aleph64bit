      SUBROUTINE FMMULX(P,AM,NRGN,IRGN,TPAR,XYZ,STH2,SR02)
C-----------------------------------------------------------------
C!  Predict mult. scattering error at extrapolated point
C.  INPUT:
C.          P = Initial track momentum
C.          AM = particle mass
C.          NRGN = No of regions crossed
C.          IRGN(NRGN) = Region numbers crossed
C.          TPAR(14,NRGN) = track parameters at each crossing
C.          XYZ(3) = Coordinates of point for evaluating
C.                   cumulative m/s.
C.  OUTPUT:
C.          STH2 = Squared mult. scat. angular error
C.          SR02 = Squared mult. scat. position error
#ifndef DOC
      SAVE
#include "fmgeom.h"
      DIMENSION IRGN(11),TPAR(14,11),XYZ(3)
C
      E = SQRT(P*P + AM*AM)
      BETA = P/E
      SCON = (0.015/(P*BETA))**2
      STH2 = 0.
      SR02 = 0.
      DO 5 I = 1,NRGN
         IR = IRGN(I)
C Path length in current region and distance to muon plane
         T = TPAR(14,I)
         D = VDIST(TPAR(1,I),XYZ(1),3)
         THE2 = SCON*T/FMRADL(IR)
         STH2 = STH2 + THE2
         SR02 = SR02 + THE2*(0.333*T*T + T*D + D*D)
    5 CONTINUE
      RETURN
      END
#endif
