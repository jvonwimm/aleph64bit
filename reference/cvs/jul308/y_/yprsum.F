      SUBROUTINE YPRSUM
C----------------------------------------------------------------------
C!Print run statistics for topology reconstruction and V0 package
C!   AUTHOR: M.A.Ciocci,L.Rolandi 13/4/88
C!   MODIFIED : M.Bosman 12/04/89
C?
C?                    Description
C?                    ===========
C?      CALL: YTOSUM, summary of the topology package
C?      CALL: YV0SUM, summary of the V0 reconstruction
C?
C?
C!======================================================================
#ifndef DOC
#include "rflags.h"
#include "rparac.h"
C
C if topology reconstruction requested call summary routine
      IF(FDETRF(JULYT)) CALL YTOSUM
C call the V0 summary
      CALL YV0SUM
C
      RETURN
      END
#endif
