      REAL FUNCTION ECTILT(DUM)
C.-------------------------------------------------------------------
CKEY ECALDES TILT ANGLE / USER
C B.Bloch
C! Get ECAL tilt
C   TILT = ECTILT(DUM)
C   Called by USER
C.-------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ecccom.h"
      ECTILT=ECALTI
      RETURN
      END
#endif
