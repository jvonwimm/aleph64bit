*DK tpirun
      SUBROUTINE TPIRUN
C -------------------------------------------------------------------
C. - Clear name indices
C. - Initialise DetectorDescription and SimulationConst
C.   for TPC simulation
C. - Get user geometry
C. - Called from         ASIMOD                     from GALEPH.HLB
C. - Calls                NAMIND, BKFMT              from BOS77.hlb
C -------------------------------------------------------
#ifndef DOC
      SAVE
#include "tpnamc.h"
#include "tpstat.h"
#include "alfgeo.h"
#include "jobcom.h"
#include "iocom.h"
      EXTERNAL NAMIND
C ----------------------------------------------------------------------
C
C  Initialize statistics array filled in TPASIG
C
      DO 10 I = 1,LTPST
         JTPSTA(I) = 0
   10 CONTINUE
C  Name indices for track TPC elements, pad hits, trigger pad hits
      NATPHT = NAMIND ('TPHT')
      NATTHT = NAMIND ('TTHT')
      NATPTE = NAMIND ('TPTE')
      NATPHE = NAMIND ('TPHE')
      NATTHE = NAMIND ('TTHE')
      NATPCO = NAMIND ('TPCO')
      NATPCH = NAMIND ('TPCH')
      NATCRL = NAMIND ('TCRL')
C
      CALL BKFMT ('TPHE','2I,(I)')
      CALL BKFMT ('TTHE','2I,(I)')
      CALL BKFMT ('TPTE','2I,(I,11F)')
      CALL BKFMT ('TTHT','2I,(2I,4F)')
      CALL BKFMT ('TPHT','2I,(2I,4F)')
      CALL BKFMT ('TPCO','2I,(I,5F,4I,2F)')
      CALL BKFMT ('TPCH','(I)')
      CALL BKFMT ('TCRL','(I)')
C
C  Get geometry banks needed from the data base
      CALL TRDDAF (LRDBIO,IRUNJO,IRET)
      IF (IRET .EQ. 0)
     &  CALL ALTELL('TPIRUN - Error accessing banks from DA file',
     &              0,'STOP')
C
C  Debug geometry and useful quantities
C
      IF (IPRIJO(3) .NE. 0) CALL TGEPRI (LOUTIO)
C
C  Full TPCSIM initialisation.
C
      IF( ICTPJO(7) .GT. 0) THEN
         CALL TGINIT (ALFIEL,ICTPJO)
         CALL TPCVER (TVERJO)
      ELSE
C        TVERJO = 0.
      ENDIF
C
      RETURN
      END
#endif
