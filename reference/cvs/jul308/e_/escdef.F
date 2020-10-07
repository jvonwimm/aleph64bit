      SUBROUTINE ESCDEF
C----------------------------------------------------------------------
C!  - Ecal Small Defect Correction
C!
C! AUTHOR : M.N MINARD 30-09-92
C!
C!
C!     Contant read from DB
C!     ECDF , EWKW
C?
C!======================================================================
#ifndef DOC
#include "rconds.h"
#include "ectcom.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "bcs.h"
      INTEGER ALGTDB, GTSTUP
C
C    Verify not a MC run
C
      KAJOB = IW(NAMIND('AJOB'))
      IF ( KAJOB.EQ.0 ) THEN
        IREADB = ALGTDB(LRCONS,'ECDF',IRUNRC)
        IF ( IREADB.NE.0) THEN
          WRITE (LOUTRL,'(A)') '    ECDF found on DAF '
        ENDIF
C
C     Look for wrong wire
C
        IREADB = ALGTDB(LRCONS,'EWKW',IRUNRC)
        IF ( IREADB.NE.0) THEN
          WRITE (LOUTRL,'(A)') '    EWKW found on DAF '
        ENDIF
      ENDIF
      RETURN
      END
#endif
