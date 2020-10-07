      SUBROUTINE KSWAP
C -----------------------------------------------------------------
C -  B.Bloch September 92
C! Swap cards read from data base to temporary ones
CKEY KINE KINGAL INIT  /   INTERNAL
C
C
C ------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
C----------------------------------------------------------------------
      WRITE (IW(6),100)
      CALL BSWAP(IW,'GMOB','KMOB')
      CALL BSWAP(IW,'GADM','KADM')
      CALL BSWAP(IW,'GRPL','KRPL')
      WRITE (IW(6),200)
 100  FORMAT(1X,/,'============= This section handle user input',
     $ ' superseding  KREF defaults ==============')
 200  FORMAT(/1X,'++++KSWAP swapped GMOB/GADM/GRPL from KREF to',
     $ ' KMOB/KADM/KRPL')
      RETURN
      END
#endif
