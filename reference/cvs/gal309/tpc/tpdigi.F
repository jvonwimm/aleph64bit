*DK tpdigi
      SUBROUTINE TPDIGI
C --------------------------------------------------------------
C - F.Ranjard - 861001
C!  Run TPCSIM (FAST version) or make fake TPCO banks.
C - Called from    ASDIGI                            from this .HLB
C - Calls          TPCOOR,TPRCOO                     from this .HLB
C -                TGTEER                            from TPCSIM.HLB
C - D.Cowen/P.Janot  30-6-88  --  Incorporate TPCSIM.
C ------------------------------------------------------
#ifndef DOC
#include "jobcom.h"
#include "jqcom.h"
#include "tpnamc.h"
C ------------------------------------------------------------------
C
C - Initialize BOS banks
      IDRP = IW(NATPCO)
      IF (IDRP .NE. 0) CALL BDROP (IW,'TPCOTPCHTCRL')
C
C - IF the filling of TPCO is requested, do it
      IF (ICTPJO(6) .GT. 0) THEN
C
C - Build Coordinate bank and relation coord-hit bank
         CALL TPCOOR
C
C - Compress the banks if necessary
         CALL AUBPRS ('TPCOTPCH')
C
C - Debug
         IF (FDEBJO .AND. IPRIJO(3).EQ.1) THEN
            CALL TPRCOO
         ENDIF
C
      ENDIF
C
C - If requested, run TPCSIM (FAST version).
C
      IF (ICTPJO(7) .GT. 0) CALL TGTEER (NEVTJO,ICTPJO)
      RETURN
      END
#endif
