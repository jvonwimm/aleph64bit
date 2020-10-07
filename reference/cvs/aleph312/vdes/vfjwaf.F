      INTEGER FUNCTION VFJWAF (JMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  First global wafer index in module
C - Joe Rothberg, December 1993
C
C - Input:
C   JMOD   / I  Global module index
C
C - Output:
C   VFJWAF / I  First global wafer index in module
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgindx.h"
C
      INTEGER JMOD
C
C ----------------------------------------------------------------------
C
      IF (JMOD .GE. 1 .AND. JMOD .LE. NMODUL) THEN
        VFJWAF = (JMOD -1)*NWAFEM + 1
      ELSE
        VFJWAF =  VDERR
      ENDIF
C
      RETURN
      END
#endif
