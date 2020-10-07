*DK vdirun
      SUBROUTINE VDIRUN
C-----------------------------------------------------------------------
C! Initialize run for VDET
CKEY VDET
C!
C!
C!  Author         F.Forti        10/6/86
C!  Modified       G.Triggiani    17/02/88
C!  Modified       A. Bonissent   10/05/94
C!
C!  Description
C!  ===========
C!  Read Data Base constants for VDET, Format hits and digits banks,
C!
C!  Called by :    ASIMOD                        from this .HLB
C!  Calls     :    BKFMT                         from BOS77
C!                 VRDDAF,VDGDMP,ALTELL          from  ALEPHLIB
C!
C---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
C
      DIMENSION IBID(2)
      INTEGER VDGDMP
      INTEGER GTSTUP,ALGTDB
      EXTERNAL NAMIND
C
C Initialize the geometry package
C
      CALL VRDDAF(LRDBIO,IRUNJO,IFLAG)
C
      IF (IFLAG.NE.1) CALL ALTELL
     &   ('VDIRUN: Return code from VRDDAF.NE.1. Stop',0,'STOP')
C
C
C Make sure that all banks necessary for digitizing are present
C
      IRUN = 1
      LDBAS = JUNIDB(0)
      IVSTP = GTSTUP ('VD',IRUN)
      IRET    = ALGTDB (LDBAS,'VDEPVPHOVDCC',IVSTP)
      KVDEP = IW(NAMIND('VDEP'))
      KVPHO = IW(NAMIND('VPHO'))
      KVDCC = IW(NAMIND('VDCC'))
      IF(KVDEP.EQ.0.OR.KVPHO.EQ.0.OR.KVDCC.EQ.0)THEN
         CALL ALTELL('VDIRUN : data base bank missing ',0,'STOP')
      ENDIF
C
C Initialize statistics variables
C
      CALL VDFILL('INIT',IBID)
C
      CALL BKFMT('VDHT','2I,(3I,7F)')
      CALL BKFMT('VHLS','I')
      CALL BKFMT('VPLH','I')
      CALL BKFMT('VDLH','2I,(6F,3I,F,2I)')
      CALL BKFMT('VTRS','2I,(I,F,2I)')
      CALL BKFMT('VDTE','2I,(2F)')
      CALL BKFMT('VWS1','2I,(F,I)')
      CALL BKFMT('VWS2','2I,(F,I)')
      CALL BKFMT('VWC1','2I,(2F,I)')
      CALL BKFMT('VWC2','2I,(2F,I)')
      CALL BKFMT('VCLU','I')
      CALL BKFMT('VTSK','2I,(F,2I)')
C
C    PRINT minivertex geometrical constants
C
      IF (IPRIJO(1).NE.0) IRET=VDGDMP()
      RETURN
      END
#endif
