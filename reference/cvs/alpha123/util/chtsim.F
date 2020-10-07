      FUNCTION CHTSIM(IVERS)
CKEY  DEDX / USER
C-----------------------------------------------------------------------
C! Check version of TPCSIM.
C If the version number stored in the  TSIM bank is 216 or higher,
C then QDEDX, rather than QDEDXM,  should be called.
C input  : none
C outputs: CHTSIM = .TRUE.  if TPCSIM version number is 216 or higher
C                 = .FALSE. if number is less than 216, or if TSIM
C                           bank cannot be found.
C          IVERS  = TPCSIM version number.
C
C M.Schmitt  28-jan-1994 initial version.
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      LOGICAL CHTSIM
      INTEGER NAMIND,NATSIM,INTSIM,IVERS,IVERS0,IDUMMY
      PARAMETER ( IVERS0 =  216)
      EXTERNAL NAMIND
#include "bcs.h"
C-----------------------------------------------------------------------
C
C get appropriate word from TSIM, and check its value.
C
      CHTSIM = .FALSE.
      NATSIM = NAMIND('TSIM')
      IF (NATSIM.GT.0) THEN
        INTSIM = IW(NATSIM)
        IF (INTSIM.GT.0) THEN
          IVERS = IW(INTSIM+3)
          IF (IVERS.GE.IVERS0) CHTSIM = .TRUE.
        ENDIF
      ENDIF
      RETURN
      END
#endif
