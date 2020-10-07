      SUBROUTINE AUHDIS(FIELD,SLONG,VI,VO)
C--------------------------------------------------------------------
C J. Hilgart 10/12/86
C This routine calculates the
C! Dist. between 2 points along a singly charged particle's helical trajectory
C
C      Input:  FIELD   in kG
C              VI(7)    xi, yi, zi, dc1, dc2, dc3,ptot
C              VO(3)    x0, y0
C      Output: SLONG, in cm
C ----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "alcons.h"
C
      DIMENSION VI(7),VO(2)
C
      XYLEN = VDIST(VI,VO,2)
            SZ = MAX(SQRT(1. - VI(6)**2),1.E-5)
      PT = VI(7)*SZ
      RHO = PT/(ABS(FIELD)*CLGHT*1.E-5)
      SLONG = 2.*RHO*ASIN(MIN(XYLEN/2./RHO,1.))/SZ
C
      RETURN
      END
#endif
