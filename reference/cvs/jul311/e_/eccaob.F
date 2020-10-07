      SUBROUTINE ECCAOB (ICL,IER)
C----------------------------------------------------------------------
C!  - Create charged em calorimeter object
C!
C!
C!  AUTHOR   : J.P. ALBANESE
C!  MODIFIED : F. ETIENNE      87/08/27
C!
C!  BANKS :
C!    INPUT   : ECLU,ECT1,ERL3,ETP1
C!    OUTPUT  : NONE
C!    CREATED : NONE
C!
C!  OUTPUT:
C!         IER = 0 O.K.
C!               1 PROBLEM WITH ECOB CREATION
C!
C------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
#include "eanclu.h"
C
C
C?   CREATE ECAL OBJECTS
      CALL ECOIDP(ICL)
      CALL ECRCOB(IER)
C
C? Compute EcObject corrected energy
      IF (IER .EQ. 0) CALL ECOENT(IER)
      RETURN
      END
#endif
