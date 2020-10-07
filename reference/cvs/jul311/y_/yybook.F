      SUBROUTINE YYBOOK
C----------------------------------------------------------------------
C
C! Book histograms for vertex fit(Mermekides),topology and V0 package
C!
C!   Author: M.Mermikides    11/10/87
C!   Modified: M.A.Ciocci,L.Rolandi 14/4/88
C!   Modified: M.Bosman 12/04/89
C?
C?                    Description
C?                    ===========
C?      CALL: YFBOOK, book histograms for main vertex (Mermekides)
C?      CALL: YTBOOK, book histograms topology package
C?      CALL: YVBOOK, book histograms for the V0 package
C?
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
C if Mermekides main vertex routine requested call booking routine
      IF(FDETRF(JULYR)) CALL YFBOOK
C if topology reconstruction requested call booking routine
      IF(FDETRF(JULYT)) CALL YTBOOK
C call V0 histogram booking
      CALL YVBOOK
C
      RETURN
      END
#endif
