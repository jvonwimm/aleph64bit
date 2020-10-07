      SUBROUTINE YINIRU
C--------------------------------------------------------------------
C! Initialise the Main Vertex(Mermekides),Topology and V0
C!
C!      AUTHOR:    M.MERMEKIDES
C!      MODIFIED:  M.A.Ciocci,L.Rolandi. 14/4/87
C!      MODIFIED:  M.BOSMAN 12/04/89
C!      MODIFIED:  M.A.Ciocci 7/6/90
C!
C!
C?                    Description
C?                    ===========
C?      CALL: YTIRUN, initialize the topology routines
C?
C?
C!======================================================================
#ifndef DOC
#include "rflags.h"
#include "rparac.h"
C
C call the initialisation routine for the topology package
      IF(FDETRF(JULYT)) CALL YTIRUN
C
      RETURN
      END
#endif
