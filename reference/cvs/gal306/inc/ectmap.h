*CD ectmap
#include "wrkspc.h"
      DIMENSION    MAPECT(LPHI,LTHET)
      EQUIVALENCE (MAPECT(1,1),WSPACE(2*LTHET+1))
      DIMENSION JPMIN(LTHET),JPMAX(LTHET)
      EQUIVALENCE (JPMIN(1),WSPACE(1)),(JPMAX(1),WSPACE(LTHET+1))
#if defined(DOC)
      MAPECT is a map of EC towers for computation
#endif
