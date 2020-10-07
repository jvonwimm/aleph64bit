      SUBROUTINE LKPRCOM(PRECOM,COMNUM)
C --------------------------------------------------
C ---------------------------------------------------
#ifndef DOC
      CHARACTER*80 PRECOM(20)
      INTEGER COMNUM
#include "lkfil.h"
C --------------------------------------------------
      DO 10 I=1,COMNUM
      L=LNBLNK(PRECOM(I))
 10   WRITE(LOUT,FMT='(I3,2A)') I,'] ',PRECOM(I)(1:L)
      RETURN
      END
#endif
