      FUNCTION PROLOT(T,ALPHA,BETA,B)
C---------------------------------------------------------------------
C!  LONGITUDINAL PROFILE OF AN E.M. SHOWER INDUCED BY ELECTRON
C!
C!  AUTHOR : A. FALVARD
C!  MODIFIED:
C---------------------------------------------------------------------
#ifndef DOC
#include "ecmode.h"
      PROLOT=(1.-B/BETA)*BETA**ALPHA/GAMMA(ALPHA)*T**(ALPHA-1.)
     **EXP(-BETA*T)+B*EXP(-BETA*T)
      RETURN
      END
#endif
