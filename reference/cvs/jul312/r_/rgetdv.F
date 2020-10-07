      SUBROUTINE RGETDV
C----------------------------------------------------------------------
C!  - Get TPC drift velocity
C!
C!   Author   :- E. Lancon             25-FEB-1992
C!   Modified :- I. Tomalin             2-AUG-1995
C!                 (Make 'ONL' option exactly equivalent to PASS0).
C!
C!   Description
C!   ===========
C!    Finds TPC drift velocity (initializes subroutine TPDVEL).
C!
C?
C!======================================================================
#ifndef DOC
#include "rpass0.h"
#include "rflags.h"
      DIMENSION DVSA (3), DVSB (3)
      CHARACTER*3 OPTION
C-----------------------------------------------------------------------
C Standard running mode of JULIA :
      OPTION = 'RAW'
      IF (FPASS0) OPTION = 'ONL'
      CALL TPDVEL ( OPTION, DVSA, DVSB, IER)
      IF (IER.EQ.0) THEN
        VIPAS0(1,1) = DVSA (1)
        VIPAS0(2,1) = DVSA (2)
        VIPAS0(3,1) = DVSA (3)
        VIPAS0(1,2) = DVSB (1)
        VIPAS0(2,2) = DVSB (2)
        VIPAS0(3,2) = DVSB (3)
      ELSE
        CALL RERROR ('RGETDV',-1,'TPC drift velocity not found.')
      END IF
      END
#endif
