      SUBROUTINE YINIJO
C----------------------------------------------------------------
C! Initialise name indices and formats bank for
C!                  Main Vertex Routine (Mermekides)
C!                  Topology software
C!                  V0 reconstruction
C!
C!     AUTHOR:  M.MERMEKIDES
C!     MODIFIED:    M.A.CIOCCI,L.ROLANDI 12/4/88
C!     MODIFIED:    M.BOSMAN  12/04/89
C!
C!
C?              Description
C?              ===========
C?     Call the initialisation routine for the 3 packages
C?     CALL YFIJOB : Main vertex (Mermekides)
C?     CALL YTIJOB : Topology reconstruction
C?     CALL YVIJOB : V0 package
C?
C?
C!======================================================================
#ifndef DOC
#include "rflags.h"
#include "rparac.h"
C
C if M.Mermekides routine requested call initialization routine
      IF(FDETRF(JULYR)) CALL YFIJOB
C if topology reconstruction requested call initialization routine
      IF(FDETRF(JULYT)) CALL YTIJOB
C initialize the V0 package
      CALL YVIJOB
      RETURN
      END
#endif
