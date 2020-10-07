      SUBROUTINE EINIJO
C----------------------------------------------------------------------
C!  - Initialisation of ECAL job
C!
C!     SET ECAL BANK INDICES:ESDA,ECLU,ECT1/2/3,ETP1/2
C!                   FORMATS
C!     INITIALIZE GEOMETRY
C!
C! Called by: RINJOB
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "ecpric.h"
C
C?  Initialyze Ecal counters for job statistics
C
      NSTOEV = 0
      ESTOEV = 0.
      NCLUEV = 0
      ECLUEV = 0.
      ECLUCH = 0.
      EWEAEV = 0.
      EWBAEV = 0.
      EWEBEV = 0.
      NELEEV = 0
      NGAMEV = 0
C
C?  SET NAME INDICES FOR ECAL BANKS
C
      CALL ECRNDX
C
C?  INITIALISATION OF "ROSE DES VENTS"
C
      CALL ECINRV
C
C?  Initialisation for neutral analysis(J Badier)
C
      CALL EBINIT(IER)
      IF (IER .NE. 0) THEN
        CALL RERROR('EINIJO ',1,
     1 ' EINIJO :Bad neutral package initialisation')
      ENDIF
      RETURN
      END
#endif
